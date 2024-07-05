{-# LANGUAGE TemplateHaskell #-}

module Blog.MMark
  ( Page (..)
  , renderMarkdown
  , renderMarkdownIO
  , md
  ) where

import           Blog.Util
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson.Types           (Object)
import           Data.Function              (fix)
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NE
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import qualified Data.Text.Lazy             as TextL
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Quote  as TH
import qualified Language.Haskell.TH.Syntax as TH
import           Lucid
import qualified Text.Megaparsec.Error      as Mega
import qualified Text.MMark                 as MMark
import           Text.MMark.Extension       as MMark
import           Text.MMark.Trans
import           Text.MMark.Type            as MMark
import           Text.MMark.Util
import qualified Text.URI                   as URI

nl :: Monad m => HtmlT m ()
nl = "\n"

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

applyBlockRender :: Monad m => RenderT m (Block (Ois, HtmlT m ())) -> Block (Ois, HtmlT m ()) -> HtmlT m ()
applyBlockRender (Render r) = fix (r . baseBlockRender)

baseBlockRender :: Monad m => (Block (Ois, HtmlT m ()) -> HtmlT m ()) -> Block (Ois, HtmlT m ()) -> HtmlT m ()
baseBlockRender blockRender = \case
  ThematicBreak ->
    hr_ [] >> nl
  Heading1 (i, html) ->
    mkHeader h1_ i html >> nl
  Heading2 (i, html) ->
    mkHeader h2_ i html >> nl
  Heading3 (i, html) ->
    mkHeader h3_ i html >> nl
  Heading4 (i, html) ->
    mkHeader h4_ i html >> nl
  Heading5 (i, html) ->
    mkHeader h5_ i html >> nl
  Heading6 (i, html) ->
    mkHeader h6_ i html >> nl
  CodeBlock _ txt -> do
    div_ [class_ "hl"] (nl <* (pre_ $ toHtml txt))
    nl
  Naked (_, html) ->
    html >> nl
  Paragraph (_, html) ->
    p_ html >> nl
  Blockquote blocks -> do
    blockquote_ (nl <* mapM_ blockRender blocks)
    nl
  OrderedList i items -> do
    let startIndex = [start_ (Text.pack $ show i) | i /= 1]
    ol_ startIndex $ do
      nl
      forM_ items $ \x -> do
        li_ (nl <* mapM_ blockRender x)
        nl
    nl
  UnorderedList items -> do
    ul_ $ do
      nl
      forM_ items $ \x -> do
        li_ (nl <* mapM_ blockRender x)
        nl
    nl
  Table calign (hs :| rows) -> do
    table_ $ do
      nl
      thead_ $ do
        nl
        tr_ $
          forM_ (NE.zip calign hs) $ \(a, h) ->
            th_ (alignStyle a) (snd h)
        nl
      nl
      tbody_ $ do
        nl
        forM_ rows $ \row -> do
          tr_ $
            forM_ (NE.zip calign row) $ \(a, h) ->
              td_ (alignStyle a) (snd h)
          nl
      nl
    nl
  Div attrs blocks -> do
    div_ (lucidAttributes attrs) (nl <* mapM_ blockRender blocks)
    nl
  where
    alignStyle = \case
      CellAlignDefault -> []
      CellAlignLeft -> [style_ "text-align:left"]
      CellAlignRight -> [style_ "text-align:right"]
      CellAlignCenter -> [style_ "text-align:center"]

applyInlineRender :: Monad m => RenderT m Inline -> (Inline -> HtmlT m ())
applyInlineRender (Render r) = fix (r . baseInlineRender)

baseInlineRender :: Monad m => (Inline -> HtmlT m ()) -> Inline -> HtmlT m ()
baseInlineRender inlineRender = \case
  Plain txt ->
    toHtml txt
  LineBreak ->
    br_ [] >> nl
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

renderHTML :: forall m. Monad m => [ExtensionT m] -> MMarkT m -> HtmlT m ()
renderHTML exts (MMark.useExtensionsM exts -> MMark {..}) =
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

data Page = Page
  { meta    :: Object
  , content :: TextL.Text
  }
  deriving (Show)

instance TH.Lift Page where
  liftTyped p = TH.unsafeCodeCoerce (TH.lift p)
  lift (Page m t) =
    liftA2 (\m t -> TH.ConE 'Page `TH.AppE` m `TH.AppE` t) (TH.lift m) (TH.lift t)

md :: TH.QuasiQuoter
md = TH.QuasiQuoter
  { quoteExp  = \x -> renderMarkdown "<quasiquote>" (Text.pack x) >>= TH.lift
  , quotePat  = \_ -> fail "illegal Page QuasiQuote"
  , quoteType = \_ -> fail "illegal Page QuasiQuote"
  , quoteDec  = \_ -> fail "illegal Page QuasiQuote"
  }

renderMarkdown :: MonadFail m => FilePath -> Text -> m Page
renderMarkdown input source = do
  case MMark.parseM input source of
    Left errs -> fail $ Mega.errorBundlePretty errs
    Right r -> do
      let meta = fromMaybe mempty $ MMark.projectYaml r
      content <- renderTextT $ renderHTML extensions r
      pure Page {..}
  where
    extensions =
      [ descriptionList
      , rawBlocks
      , demoteHeaders
      ]

renderMarkdownIO :: (MonadFail m, MonadIO m) => FilePath -> m Page
renderMarkdownIO file = do
  input <- liftIO $ Text.readFile file
  renderMarkdown file input

-- scuffed implementation of description lists
descriptionList :: MonadFail m => ExtensionT m
descriptionList = blockRenderM \old -> \case
  Div attrs blocks | elem "dl" (MMark.classes attrs) -> do
    dl_ $ forM_ (pairUp blocks) \(x, y) -> do
      dt_ (old x)
      dd_ (old y)
    nl
  x -> old x
  where
    pairUp = reverse . fst . foldl go ([], Nothing)

    go (acc, Nothing) fst  = (acc, Just fst)
    go (acc, Just fst) snd = ((fst, snd) : acc, Nothing)

-- emit blocks with language 'raw' as raw HTML
rawBlocks :: MonadFail m => ExtensionT m
rawBlocks = blockRenderM \old -> \case
  CodeBlock (Just "{=raw}") txt -> toHtmlRaw txt
  x -> old x

-- demote headers by 1 (h1 -> h2, ..., h6 -> p)
demoteHeaders :: MonadFail m => ExtensionT m
demoteHeaders = blockTransM \case
  Heading1 x -> pure $ Heading2 x
  Heading2 x -> pure $ Heading3 x
  Heading3 x -> pure $ Heading4 x
  Heading4 x -> pure $ Heading5 x
  Heading5 x -> pure $ Heading6 x
  Heading6 x -> pure $ Paragraph x
  x -> pure x
