{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Blog.Shake
    ( GitHash (..)
    , PostA (..)
    , PostQ (..)
    , Route (..)
    , defaultExtensions
    , forP_
    , gitHashOracle
    , mapP
    , noteEntry
    , postExtensions
    , renderMarkdown
    , renderMarkdownIO
    , route
    , routePage
    , routeStatic
    , routeStatic1
    , staticFiles
    , staticFiles'
    , wrapPostQ
    , writeFile
    , writePage
    ) where

import Blog.Config                (siteOutput)
import Blog.MMark                 qualified as MMark
import Blog.Template              (Template)
import Blog.Template              qualified as Template
import Blog.Type
import Blog.Util
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Aeson                 qualified as Aeson
import Data.ByteString.Char8      qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable              (toList)
import Data.Map.Strict            qualified as Map
import Data.String                (IsString (..))
import Data.Text                  qualified as Text
import Data.Text.Encoding         qualified as Text
import Data.Text.Lazy.IO          qualified as TextL
import Data.Time
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath ((<.>), (</>))
import Development.Shake.FilePath qualified as FP
import GHC.Stack
import GHC.SyntaxHighlighter      (Token (..), tokenizeHaskell)
import Lucid
import Prelude                    hiding (writeFile)
import System.Directory           qualified as Dir
import Text.MMark.Extension       as MMark
import Text.MMark.Type            as MMark

forP_ :: Foldable t => t a -> (a -> Action b) -> Action ()
forP_ t f = void $ forP (toList t) f
{-# INLINE forP_ #-}

mapP :: (a -> Action b) -> [a] -> Action [b]
mapP = flip forP
{-# INLINE mapP #-}

writeFile :: HasCallStack => FilePath -> LazyByteString -> Action ()
writeFile output contents = do
  liftIO do
    Dir.createDirectoryIfMissing True (FP.takeDirectory output)
    LBS.writeFile output contents

writePage :: HasCallStack => Aeson.Value -> Template -> FilePath -> Page -> Action ()
writePage meta template output page =
  liftIO do
    Dir.createDirectoryIfMissing True (FP.takeDirectory output)
    let contents = Template.renderPage meta template page
    TextL.writeFile output contents

data Route
  = Dynamic (FilePath -> FilePath) FilePattern
  | Static Bool (Maybe FilePath) FilePath

instance IsString Route where
  fromString = Static False Nothing

route :: Route -> (FilePath -> FilePath -> Action ()) -> Rules ()
route (Static isPat input output) f = do
  unless isPat $ want [siteOutput </> output]
  siteOutput </> output %> \x -> case input of
    Just input | not isPat -> need [input] *> f input x
    _                      -> f x x
route (Dynamic g pat) f = do
  let getOut x = siteOutput </> g x

  -- split into 2 steps to avoid indirect recursion
  action $ getDirectoryFiles "" [pat] >>= need . fmap getOut

  outputMap <- liftAction $
    Map.fromList . fmap (\x -> (getOut x, x)) <$> getDirectoryFiles "" [pat]

  getOut pat %> \output -> do
    input <- (Map.! output) <$> outputMap
    need [input]
    f input output

staticFiles' :: FilePattern -> (FilePath -> FilePath) -> Rules ()
staticFiles' p k = flip route f $ Dynamic k p
  where
    f input output = do
      putInfo $ unwords ["STATIC", output]
      copyFileChanged input output

staticFiles :: FilePattern -> Rules ()
staticFiles = flip staticFiles' id

routePage :: FilePath -> (FilePath -> FilePath -> Action ()) -> Rules ()
routePage = route . Dynamic (\input -> FP.takeBaseName input <.> "html")

routeStatic1 :: FilePattern -> (FilePath -> Action ()) -> Rules ()
routeStatic1 output f = do
  let outputFile = siteOutput </> output
  outputFile %> f
  want [outputFile]

routeStatic :: FilePath -> FilePath -> (FilePath -> FilePath  -> Action ()) -> Rules ()
routeStatic input output f = do
  let outputFile = siteOutput </> output
  outputFile %> \output -> need [input] *> f input output
  want [outputFile]

ghcHighlight :: Monad m => StrictText -> Maybe (HtmlT m ())
ghcHighlight (tokenizeHaskell -> Just x) = pure $
  forM_ x \(c, toHtml -> h) ->
    case tokenClass c of
      Just c  -> span_ [class_ c] h
      Nothing -> span_ h
  where
    tokenClass :: Token -> Maybe StrictText
    tokenClass = \case
      KeywordTok     -> Just "k"
      PragmaTok      -> Just "na"
      SymbolTok      -> Just "p"
      VariableTok    -> Just "nv"
      ConstructorTok -> Just "kt"
      OperatorTok    -> Just "o"
      CharTok        -> Just "sc"
      StringTok      -> Just "s2"
      IntegerTok     -> Just "m"
      RationalTok    -> Just "mf"
      CommentTok     -> Just "c"
      SpaceTok       -> Nothing
      OtherTok       -> Just "x"
ghcHighlight _ = Nothing

highlight :: Extension Action
highlight = blockRender \old block -> case block of
  CodeBlock (Just "haskell") txt ->
    case ghcHighlight txt of
      Just html -> wrap html
      Nothing   -> old block
  CodeBlock (Just l) txt -> do
    html <- lift $ pygmentize l (Text.encodeUtf8 txt)
    wrap $ toHtmlRaw html
  _ -> old block
 where
   pygmentize :: StrictText -> StrictByteString -> Action StrictByteString
   pygmentize lang raw = fromStdout <$> cmd @(CmdOption -> [String] -> Action _)
     (StdinBS (BS.fromStrict raw))
     [ "pygmentize", "-l", Text.unpack lang, "-f", "html", "-O", "nowrap=True"]

   wrap :: Monad m => HtmlT m () -> HtmlT m ()
   wrap x = div_ [class_ "hl"] ("\n" <* pre_ x)

noteEntry :: Extension Action
noteEntry = demote <> renderNote
  where
    decode = Aeson.decode
      . LBS.fromStrict
      . (\x -> BS.cons '"' (BS.snoc x '"'))
      . Text.encodeUtf8

    renderNote = blockRender \old block -> case block of
      Div attrs (title : body) | "note" `elem` attrs.classes ->
        case attrs.pairs Map.!? "date" >>= decode of
          Just (MkTime t) -> do
            article_ $ do
              old title
              "\n"
              div_ [class_ "post-info"] . ul_ . li_ . toHtml $
                formatTime defaultTimeLocale "%e %B %Y" t
              "\n"
              mapM_ old body
            "\n"
          Nothing -> old block
      _ -> old block

    demote = blockTrans \case
      Heading1 x -> pure $ Heading3 x
      Heading2 x -> pure $ Heading4 x
      Heading3 x -> pure $ Heading5 x
      Heading4 x -> pure $ Heading6 x
      Heading5 x -> pure $ Paragraph x
      Heading6 x -> pure $ Paragraph x
      x -> pure x

defaultExtensions :: [Extension Action]
defaultExtensions =
  [ MMark.rawBlocks
  , MMark.descriptionList
  , MMark.exposeRaw
  , MMark.prettifyPlain
  , highlight
  ]

postExtensions :: [Extension Action]
postExtensions = MMark.demoteHeaders : defaultExtensions

renderMarkdown :: FilePath -> StrictText -> Action Page
renderMarkdown = MMark.renderMarkdown defaultExtensions

renderMarkdownIO :: FilePath -> Action Page
renderMarkdownIO = MMark.renderMarkdownIO defaultExtensions

newtype GitHash
  = GitHash String
  deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

type instance RuleResult GitHash = StrictText

gitHashOracle :: Rules (String -> Action StrictText)
gitHashOracle = fmap (. GitHash) . addOracle $ \(GitHash branch) -> Text.strip . Text.decodeUtf8 . fromStdout <$>
  cmd @(String -> [String] -> Action _) "git" ["rev-parse", "--short", branch]

newtype PostQ
  = PostQ FilePath
  deriving (Binary, Eq, Hashable, NFData, Typeable)

instance Show PostQ where
  show (PostQ p) = show p

newtype PostA
  = PostA { unPostA :: (Post, Page) }
  deriving (Binary, Eq, Hashable, NFData, Typeable)

instance Show PostA where
  show (PostA (p, _)) = show p

type instance RuleResult PostQ = PostA

wrapPostQ :: (PostQ -> Action PostA) -> FilePath -> Action (Post, Page)
wrapPostQ k = fmap (.unPostA) . k . PostQ
