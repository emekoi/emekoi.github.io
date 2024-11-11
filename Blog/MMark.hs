module Blog.MMark
    ( demoteHeaders
    , descriptionList
    , exposeRaw
    , md
    , prettifyPlain
    , rawBlocks
    , renderMarkdown
    , renderMarkdownIO
    , details
    ) where

import Blog.Type                  (Page (..), StrictText, fileError)
import Blog.Util
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson                 (toJSON, (.=))
import Data.Aeson.KeyMap          qualified as Aeson
import Data.Foldable              (foldl', foldrM)
import Data.Function              (fix)
import Data.IORef                 (IORef)
import Data.IORef                 qualified as IORef
import Data.List                  qualified as List
import Data.List.NonEmpty         (NonEmpty (..))
import Data.List.NonEmpty         qualified as NE
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Text                  qualified as Text
import Data.Text.IO               qualified as Text
import Data.Text.Lazy             qualified as TextL
import Language.Haskell.TH.Quote  qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Lucid
import Text.Megaparsec.Error      qualified as Mega
import Text.MMark                 qualified as MMark
import Text.MMark.Extension       as MMark
import Text.MMark.Trans
import Text.MMark.Type            as MMark
import Text.MMark.Util
import Text.URI                   qualified as URI

data RenderState m = RenderState
  { noteMap :: Map StrictText (Int -> HtmlT m ())
  , noteRef :: [StrictText]
  }

nl :: Monad m => HtmlT m ()
nl = "\n"

fn :: Int -> StrictText
fn x = "fn:" <> Text.pack (show x)

fnref :: Int -> StrictText
fnref x = "fnref:" <> Text.pack (show x)

fnrefBack :: Monad m => Int -> HtmlT m ()
fnrefBack n =
  a_ [ href_ $ "#" <> fnref n
     , role_ "doc-backlink"
     ] "↩"

mkHeader
  :: Monad m
  => ([Attribute] -> HtmlT m () -> HtmlT m ())
  -> Ois -> HtmlT m () -> HtmlT m ()
mkHeader f i h = f [id_ anchor] $
  a_ [href_ $ URI.render link, class_ "anchor"] mempty *> h
  where
    anchor = titleSlug (asPlainText $ getOis i)
    link = URI.URI
        { uriScheme = Nothing,
          uriAuthority = Left False,
          uriPath = Nothing,
          uriQuery = [],
          uriFragment = URI.mkFragment anchor
        }

applyBlockRender
  :: MonadIO m
  => IORef (RenderState m) -> Render m (Block (Ois, HtmlT m ()))
  -> Block (Ois, HtmlT m ()) -> HtmlT m ()
applyBlockRender stateRef (Endo r) = fix (r . baseBlockRender stateRef)

baseBlockRender
  :: MonadIO m
  => IORef (RenderState m) -> (Block (Ois, HtmlT m ()) -> HtmlT m ())
  -> Block (Ois, HtmlT m ()) -> HtmlT m ()
baseBlockRender stateRef blockRender = \case
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
    pre_ (code_ $ toHtml txt) >> nl
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
  Div attrs blocks
    | "ignore" `elem` attrs.classes -> pure ()
    | otherwise -> do
      div_ (lucidAttributes attrs) do
        nl
        mapM_ blockRender blocks
      nl
  Note label blocks -> do
    liftIO $ IORef.modifyIORef' stateRef \RenderState{..} -> do
      case List.unsnoc blocks of
        Nothing ->
          RenderState
            { noteMap = Map.insert label (\_ -> pure ()) noteMap
            , ..
            }
        Just (sx, x) ->
          RenderState
            { noteMap = Map.insert label
                (\n -> forM_ sx blockRender *> rBacklink n x)
                noteMap
            , ..
            }
  where
    alignStyle = \case
      CellAlignDefault -> []
      CellAlignLeft -> [style_ "text-align:left"]
      CellAlignRight -> [style_ "text-align:right"]
      CellAlignCenter -> [style_ "text-align:center"]

    rBacklink n (Paragraph (i, x)) =
      blockRender $ Paragraph (i, x *> toHtmlRaw @StrictText "&nbsp;" *> fnrefBack n)
    rBacklink n b =
      blockRender b *> fnrefBack n

applyInlineRender
  :: MonadIO m
  => IORef (RenderState m) -> Render m Inline
  -> (Inline -> HtmlT m ())
applyInlineRender stateRef (Endo r) = fix (r . baseInlineRender stateRef)

baseInlineRender
  :: MonadIO m
  => IORef (RenderState m) -> (Inline -> HtmlT m ())
  -> Inline -> HtmlT m ()
baseInlineRender stateRef inlineRender = \case
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
  Link inner uri mtitle ->
      let title = maybe [] (pure . title_) mtitle in
        a_ (href_ (URI.render uri) : title) (mapM_ inlineRender inner)
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
  NoteRef label -> do
    RenderState{..} <- liftIO $ IORef.readIORef stateRef
    let n = 1 + length noteRef
    sup_ [id_ $ fnref n, role_ "doc-noteref"] $
      a_ [class_ "footnote", href_ $ "#" <> fn n]
         (toHtml . Text.pack $ show n)
    liftIO $ IORef.writeIORef stateRef RenderState
        { noteRef = label : noteRef
        , ..
        }

renderHTML
  :: (Monad m, MonadIO m)
  => [Extension m] -> MMark m -> HtmlT m ()
renderHTML exts (MMark.useExtensions exts -> MMark {..}) = do
  stateRef <- liftIO $ IORef.newIORef (RenderState mempty [])
  mapM_ (rBlock stateRef) mmarkBlocks
  state <- liftIO $ IORef.readIORef stateRef
  unless (null state.noteRef) do
    hr_ []
    section_ [id_ "footnotes", role_ "doc-endnotes"] $ ol_ $
      void $ foldrM `flip` (1 :: Int) `flip` state.noteRef $ \label n -> do
        let
          body = state.noteMap Map.! label
        li_ [id_ $ fn n] do
          body n
        pure (n + 1)
  where
    Extension {..} = mmarkExtension

    rBlock stateRef x0 = do
      x1 <- lift $ applyBlockTrans extBlockTrans x0
      x2 <- lift $ traverse (rInlines stateRef) x1
      applyBlockRender stateRef extBlockRender x2

    rInlines stateRef x0 = do
      x1 <- traverse (applyInlineTrans extInlineTrans) x0
      pure $ (mkOisInternal &&& mapM_ (applyInlineRender stateRef extInlineRender)) x1

md :: TH.QuasiQuoter
md = TH.QuasiQuoter
  { quoteExp  = \x -> renderMarkdown [] "<quasiquote>" (Text.pack x) >>= TH.lift
  , quotePat  = \_ -> fail "illegal Page QuasiQuote"
  , quoteType = \_ -> fail "illegal Page QuasiQuote"
  , quoteDec  = \_ -> fail "illegal Page QuasiQuote"
  }

renderMarkdown :: (MonadFail m, MonadIO m) => [Extension m] -> FilePath -> StrictText -> m Page
renderMarkdown exts input source = do
  case MMark.parse input source of
    Left errs -> fileError (Just input) $ Mega.errorBundlePretty errs
    Right r -> do
      let meta = loadMeta r (MMark.projectYaml r)
      body <- fmap TextL.strip . renderTextT $ renderHTML exts r
      pure Page {..}
  where
    loadMeta r m = MMark.fold r (\x b -> x || any (any isMath) b) False \b ->
      let katex = toJSON b in
        maybe ("katex" .= katex) (Aeson.insert "katex" katex) m
    isMath Math{} = True
    isMath _      = False

renderMarkdownIO :: (MonadFail m, MonadIO m) => [Extension m] -> FilePath -> m Page
renderMarkdownIO exts file = do
  input <- liftIO $ Text.readFile file
  liftIO . putStrLn $ unwords [ "READ", file ]
  renderMarkdown exts file input

-- "details" tag
details :: Monad m => Extension m
details = blockRender \old -> \case
  Div attrs [summary,body] | "details" `elem` attrs.classes -> do
    details_ $ do
      summary_ (getSummary summary)
      old body
    "\n"
  Div attrs [body] | "details" `elem` attrs.classes -> do
    details_ $ do
      old body
    "\n"
  x -> old x
  where
    getSummary (Paragraph (_, p)) = p
    getSummary _ = error "invalid details summary"

-- scuffed implementation of description lists
descriptionList :: Monad m => Extension m
descriptionList = blockRender \old -> \case
  Div attrs blocks | "dl" `elem` attrs.classes -> do
    dl_ $ forM_ (pairUp blocks) \(x, y) -> do
      dt_ (old x)
      dd_ (old y)
    "\n"
  x -> old x
  where
    pairUp = reverse . fst . foldl' go ([], Nothing)

    go (acc, Nothing) fst  = (acc, Just fst)
    go (acc, Just fst) snd = ((fst, snd) : acc, Nothing)

-- emit blocks with language 'raw' as raw HTML
rawBlocks :: Monad m => Extension m
rawBlocks = blockRender \old -> \case
  CodeBlock attrs txt | ["{=raw}"] <- attrs.classes -> toHtmlRaw txt
  x -> old x

-- demote headers by 1 (h1 -> h2, ..., h6 -> p)
demoteHeaders :: Monad m => Extension m
demoteHeaders = blockTrans \case
  Heading1 x -> pure $ Heading2 x
  Heading2 x -> pure $ Heading3 x
  Heading3 x -> pure $ Heading4 x
  Heading4 x -> pure $ Heading5 x
  Heading5 x -> pure $ Heading6 x
  Heading6 x -> pure $ Paragraph x
  x -> pure x

-- expose singleton raw blocks
exposeRaw :: Monad m => Extension m
exposeRaw = blockTrans \case
  Paragraph i@(RawInline _ :| []) -> pure $ Naked i
  x -> pure x

prettifyPlain :: Monad m => Extension m
prettifyPlain = inlineTrans \case
  Plain x -> pure . Plain $ Text.unfoldr go x
  x -> pure x
  where
    go i = case Text.uncons i of
      Just ('.', i) ->
        case Text.splitAt 2 i of
          ("..", i) -> Just ('…', i)
          _         -> Just ('.', i)
      Just ('-', i) ->
        case Text.splitAt 2 i of
          ("--", i) -> Just ('—', i)
          _ ->
            case Text.splitAt 1 i of
              ("-", i) -> Just ('–', i)
              _        -> Just ('-', i)
      x -> x
