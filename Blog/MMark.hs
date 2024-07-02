{-# LANGUAGE QuasiQuotes #-}

module Blog.MMark
    ( Document (..)
    , Blog.MMark.parse
    ) where

import           Blog.Slug
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson.Types      (Value (..))
import           Data.Char             (isSpace)
import           Data.Function         (fix)
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.List.NonEmpty    as NE
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Lazy        as TextL
import           Lucid
import qualified Text.Megaparsec.Error as Mega
import qualified Text.MMark            as MMark
import           Text.MMark.Extension  as MMark
import           Text.MMark.Trans
import           Text.MMark.Type       as MMark
import           Text.MMark.Util
import qualified Text.URI              as URI

newline :: Monad m => HtmlT m ()
newline = "\n"

mkHeader :: Monad m => ([Attribute] -> HtmlT m () -> HtmlT m ()) -> Ois -> HtmlT m () -> HtmlT m ()
mkHeader f i h = f [id_ anchor] (a_ [href_ $ URI.render link] h)
  where
    anchor = titleSlug (asPlainText $ getOis i)
    link = URI.URI
        { uriScheme = Nothing,
          uriAuthority = Left False,
          uriPath = Nothing,
          uriQuery = [],
          uriFragment = URI.mkFragment anchor
        }

applyBlockRender :: Monad m => Render m (Block (Ois, HtmlT m ())) -> Block (Ois, HtmlT m ()) -> HtmlT m ()
applyBlockRender (Render r) = fix (r . baseBlockRender)

baseBlockRender :: Monad m => (Block (Ois, HtmlT m ()) -> HtmlT m ()) -> Block (Ois, HtmlT m ()) -> HtmlT m ()
baseBlockRender blockRender = \case
  ThematicBreak ->
    hr_ [] >> newline
  Heading1 (i, html) ->
    mkHeader h1_ i html >> newline
  Heading2 (i, html) ->
    mkHeader h2_ i html >> newline
  Heading3 (i, html) ->
    mkHeader h3_ i html >> newline
  Heading4 (i, html) ->
    mkHeader h4_ i html >> newline
  Heading5 (i, html) ->
    mkHeader h5_ i html >> newline
  Heading6 (i, html) ->
    mkHeader h6_ i html >> newline
  CodeBlock infoString txt -> do
    let f x = class_ $ "language-" <> Text.takeWhile (not . isSpace) x
    pre_ $ code_ (maybe [] (pure . f) infoString) (toHtml txt)
    newline
  Naked (_, html) ->
    html >> newline
  Paragraph (_, html) ->
    p_ html >> newline
  Blockquote blocks -> do
    blockquote_ (newline <* mapM_ blockRender blocks)
    newline
  OrderedList i items -> do
    let startIndex = [start_ (Text.pack $ show i) | i /= 1]
    ol_ startIndex $ do
      newline
      forM_ items $ \x -> do
        li_ (newline <* mapM_ blockRender x)
        newline
    newline
  UnorderedList items -> do
    ul_ $ do
      newline
      forM_ items $ \x -> do
        li_ (newline <* mapM_ blockRender x)
        newline
    newline
  Table calign (hs :| rows) -> do
    table_ $ do
      newline
      thead_ $ do
        newline
        tr_ $
          forM_ (NE.zip calign hs) $ \(a, h) ->
            th_ (alignStyle a) (snd h)
        newline
      newline
      tbody_ $ do
        newline
        forM_ rows $ \row -> do
          tr_ $
            forM_ (NE.zip calign row) $ \(a, h) ->
              td_ (alignStyle a) (snd h)
          newline
      newline
    newline
  Div attrs blocks ->
    div_ (lucidAttributes attrs) (mapM_ blockRender blocks)
  where
    alignStyle = \case
      CellAlignDefault -> []
      CellAlignLeft -> [style_ "text-align:left"]
      CellAlignRight -> [style_ "text-align:right"]
      CellAlignCenter -> [style_ "text-align:center"]

applyInlineRender :: Monad m => Render m Inline -> (Inline -> HtmlT m ())
applyInlineRender (Render r) = fix (r . baseInlineRender)

baseInlineRender :: Monad m => (Inline -> HtmlT m ()) -> Inline -> HtmlT m ()
baseInlineRender inlineRender = \case
  Plain txt ->
    toHtml txt
  LineBreak ->
    br_ [] >> newline
  Emphasis inner ->
    em_ (mapM_ inlineRender inner)
  Strong inner ->
    strong_ (mapM_ inlineRender inner)
  Strikeout inner ->
    del_ (mapM_ inlineRender inner)
  Subscript inner ->
    sub_ (mapM_ inlineRender inner)
  Superscript inner ->
    sup_ (mapM_ inlineRender inner)
  CodeSpan txt -> code_ (toHtml txt)
  Link inner dest mtitle ->
    let title = maybe [] (pure . title_) mtitle in
      a_ (href_ (URI.render dest) : title) (mapM_ inlineRender inner)
  Image desc src mtitle ->
    let title = maybe [] (pure . title_) mtitle in
      img_ (alt_ (asPlainText desc) : src_ (URI.render src) : loading_ "lazy" : title)
  RawInline txt ->
    toHtmlRaw txt
  Math t txt ->
    let c = case t of InlineMath -> "math inline"; DisplayMath -> "math display"
      in span_ [class_ c] (toHtmlRaw txt)
  Span attrs inner ->
    span_ (lucidAttributes attrs) (mapM_ inlineRender inner)

render :: forall m. Monad m => [Extension m] -> MMark m -> HtmlT m ()
render exts (MMark.useExtensions exts -> MMark {..}) =
  mapM_ rBlock mmarkBlocks
  where
    Extension {..} = mmarkExtension

    rBlock :: Monad m => Bni -> HtmlT m ()
    rBlock x0 = do
      x1 :: Block (NonEmpty Inline) <- lift $ applyBlockTrans extBlockTrans x0
      x2 :: Block (Ois, HtmlT m ()) <- lift $ traverse (\r -> rInlines r) x1
      applyBlockRender extBlockRender x2

    rInlines :: Monad m => NonEmpty Inline -> m (Ois, HtmlT m ())
    rInlines x0 = do
      x1 <- traverse (applyInlineTrans extInlineTrans) x0
      pure $ (mkOisInternal &&& mapM_ (applyInlineRender extInlineRender)) x1

data Document = Doc
  { meta :: Value
  , body :: TextL.Text
  }
  deriving (Show)

parse :: MonadFail m => Text -> Text -> m Document
parse (Text.unpack -> input) source = do
  case MMark.parse input source of
    Left errs -> fail $ Mega.errorBundlePretty errs
    Right r -> do
      let meta = fromMaybe Null $ MMark.projectYaml r
      body <- renderTextT $ render extensions r
      pure Doc {..}
  where
    extensions = []
