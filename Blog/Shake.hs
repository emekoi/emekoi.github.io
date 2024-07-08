{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Blog.Shake
  ( GitHash (..)
  , PostQ (..)
  , PostA (..)
  , Route (..)
  , defaultExtensions
  , forP_
  , gitHashOracle
  , mapP
  , postExtensions
  , renderMarkdown
  , renderMarkdownIO
  , route
  , routePage
  , routeStatic
  , routeStatic1
  , staticFiles
  , writeFile
  , writePage
  ) where

import           Blog.Config                (siteOutput)
import qualified Blog.MMark                 as MMark
import           Blog.Template              (Template)
import qualified Blog.Template              as Template
import           Blog.Type
import           Blog.Util
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.Foldable              (toList)
import qualified Data.Map.Strict            as Map
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Lazy.Encoding    as TextL
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath ((<.>), (</>))
import qualified Development.Shake.FilePath as FP
import           GHC.Stack
import           GHC.SyntaxHighlighter      (Token (..), tokenizeHaskell)
import           Lucid
import           Prelude                    hiding (writeFile)
import qualified System.Directory           as Dir
import           Text.MMark.Extension       as MMark

forP_ :: Foldable t => t a -> (a -> Action b) -> Action ()
forP_ t f = void $ forP (toList t) f
{-# INLINE forP_ #-}

mapP :: (a -> Action b) -> [a] -> Action [b]
mapP = flip forP
{-# INLINE mapP #-}

writeFile :: HasCallStack => FilePath -> LBS.ByteString -> Action ()
writeFile output contents = do
  liftIO do
    Dir.createDirectoryIfMissing True (FP.takeDirectory output)
    LBS.writeFile output contents

writePage :: HasCallStack => Aeson.Value -> Template -> FilePath -> Page -> Action ()
writePage meta template output = writeFile output
  . TextL.encodeUtf8
  . Template.renderPage meta template

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

staticFiles :: FilePattern -> Rules ()
staticFiles = flip route f . Dynamic id
  where
    f input output = do
      putInfo $ unwords ["STATIC", output]
      copyFileChanged input output

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

ghcHighlight :: Monad m => Text -> Maybe (HtmlT m ())
ghcHighlight (tokenizeHaskell -> Just x) = pure $
  forM_ x \(c, toHtml -> h) ->
    case tokenClass c of
      Just c  -> span_ [class_ c] h
      Nothing -> span_ h
  where
    tokenClass :: Token -> Maybe Text
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

highlight :: ExtensionT Action
highlight = blockRenderM \old block -> case block of
  CodeBlock (Just "haskell") txt ->
    case ghcHighlight txt of
      Just html -> wrap html
      Nothing   -> old block
  CodeBlock (Just l) txt -> do
    html <- lift $ pygmentize l (Text.encodeUtf8 txt)
    wrap $ toHtmlRaw html
  _ -> old block
 where
   pygmentize :: Text -> BS.ByteString -> Action BS.ByteString
   pygmentize lang raw = fromStdout <$> cmd @(CmdOption -> [String] -> Action _)
     (StdinBS (BS.fromStrict raw))
     [ "pygmentize", "-l", Text.unpack lang, "-f", "html", "-O", "nowrap=True"]

   wrap :: Monad m => HtmlT m () -> HtmlT m ()
   wrap x = div_ [class_ "hl"] ("\n" <* pre_ x)

defaultExtensions :: [ExtensionT Action]
defaultExtensions =
  [ MMark.rawBlocks
  , MMark.descriptionList
  , highlight
  ]

postExtensions :: [ExtensionT Action]
postExtensions = MMark.demoteHeaders : defaultExtensions

renderMarkdown :: FilePath -> Text -> Action Page
renderMarkdown = MMark.renderMarkdown defaultExtensions

renderMarkdownIO :: FilePath -> Action Page
renderMarkdownIO = MMark.renderMarkdownIO defaultExtensions

newtype GitHash = GitHash String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult GitHash = Text

gitHashOracle :: Rules (String -> Action Text)
gitHashOracle = fmap (. GitHash) . addOracle $ \(GitHash branch) -> Text.strip . Text.decodeUtf8 . fromStdout <$>
  cmd @(String -> [String] -> Action _) "git" ["rev-parse", "--short", branch]

newtype PostQ = PostQ FilePath
  deriving (Typeable, Eq, Hashable, Binary, NFData)

instance Show PostQ where
  show (PostQ p) = show p

newtype PostA = PostA { unPostA :: (Post, Page) }
  deriving (Typeable, Eq, Hashable, Binary, NFData)

instance Show PostA where
  show (PostA (p, _)) = show p

type instance RuleResult PostQ = PostA
