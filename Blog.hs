{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Blog (main) where

import           Blog.MMark                 (Page)
import qualified Blog.MMark                 as MMark
import           Blog.Type
import           Blog.Template              (Template)
import qualified Blog.Template              as Template
import           Blog.Util
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.List                  as List
import qualified Data.Map.Strict            as Map
import qualified Data.Ord                   as Ord
import qualified Data.Set                   as Set
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Lazy             as TextL
import qualified Data.Text.Lazy.Encoding    as TextL
import           Development.Shake          hiding (shakeOptions)
import qualified Development.Shake          as Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath ((<.>), (</>))
import qualified Development.Shake.FilePath as FP
import           GHC.Conc                   (numCapabilities)
import           GHC.Stack
import qualified Options.Applicative        as A
import           Prelude                    hiding (writeFile)
import qualified System.Directory           as Dir

data Options = Options
  { command   :: Command
  , verbosity :: Verbosity
  , jobs      :: Int
  }

newtype BuildOptions = BuildOptions
  { _buildDrafts :: Bool
  }

data Command
  = Build BuildOptions
  | Clean

shakeOptions :: Options -> IO ShakeOptions
shakeOptions Options{..} = do
  version <- getHashedShakeVersion ["Blog.hs"]

  pure $ Shake.shakeOptions
    { shakeVerbosity = verbosity
    , shakeThreads   = jobs
    , shakeColor     = True
    , shakeFiles     = "_build"
    , shakeVersion   = version
    }

writeFile :: (MonadIO m, HasCallStack) => FilePath -> LBS.ByteString -> m ()
writeFile name x = liftIO $ do
  Dir.createDirectoryIfMissing True (FP.takeDirectory name)
  LBS.writeFile name x

writePage :: (MonadIO m, HasCallStack) => Aeson.Value -> Template -> FilePath -> Page -> m ()
writePage meta template output = writeFile output
  . TextL.encodeUtf8
  . Template.renderPage meta template

data Route
  = Dynamic (FilePath -> FilePath) FilePattern
  | Static Bool (Maybe FilePath) FilePath

instance IsString Route where
  fromString = Static False Nothing

getOutputRules :: FilePath -> Rules FilePath
getOutputRules file = do
  buildDir <- shakeFiles <$> getShakeOptionsRules
  pure $ buildDir </> file

route :: Route -> (FilePath -> FilePath -> Action ()) -> Rules ()
route (Static isPat input output) f = do
  output <- getOutputRules output
  unless isPat $ want [output]
  output %> \x -> case input of
    Just input | not isPat -> need [input] *> f input x
    _                      -> f x x
route (Dynamic g pat) f = do
  buildDir <- shakeFiles <$> getShakeOptionsRules
  let getOut x = buildDir </> g x

  -- split into 2 steps to avoid indirect recursion
  action $ getDirectoryFiles "" [pat] >>= need . fmap getOut

  outputMap <- liftAction $
    Map.fromList . fmap (\x -> (getOut x, x)) <$> getDirectoryFiles "" [pat]

  getOut pat %> \output -> do
    input <- (Map.! output) <$> outputMap
    need [input]
    f input output

routeStatic :: FilePattern -> Rules ()
routeStatic = flip route copyFileChanged . Dynamic id

routePage :: FilePath -> (FilePath -> FilePath -> Action ()) -> Rules ()
routePage = route . Dynamic (\input -> FP.takeBaseName input <.> "html")

routePage' :: FilePath -> FilePath -> (FilePath -> FilePath -> Action ()) -> Rules ()
routePage' input output = route (Static False (Just input) output)

tagsMeta :: Site -> Aeson.Value
tagsMeta = Aeson.toJSON . Map.foldrWithKey' f [] . tagMap
  where
    f (Tag k) v = (Aeson.object ["tag" .= ("#" <> k), "site" .= Aeson.Object ("posts" .= v)] :)
    tagMap Site{..} =
      foldr (\p posts -> Set.foldl' (\posts t -> Map.adjust (p:) t posts) posts p.tags)
      -- foldr (\p posts -> Set.foldl' (flip (Map.adjust (p :))) posts p.tags)
        (Map.fromAscList . map (, []) $ Set.toAscList tags)
        posts

newtype GitHash = GitHash String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult GitHash = Text

gitHashOracle :: Rules (String -> Action Text)
gitHashOracle = fmap (. GitHash) . addOracle $ \(GitHash branch) -> Text.strip . Text.decodeUtf8 . fromStdout <$>
  cmd @(String -> [String] -> Action _) "git" ["rev-parse", "--short", branch]

newtype BuildPost = BuildPost String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult BuildPost = (Post, TextL.Text)

extensions :: [ExtensionT Action]
extensions =
  [ rawBlocks
  , descriptionList
  , demoteHeaders
  , highlight
  ]

renderMarkdown :: FilePath -> Text -> Action Page
renderMarkdown = MMark.renderMarkdown extensions

renderMarkdownIO :: FilePath -> Action Page
renderMarkdownIO = MMark.renderMarkdownIO extensions

-- TODO: copy post static files
-- TODO: extended posts
-- TODO: build resume
-- TODO: build atom feed
-- TODO: build jsonfeed feed
-- TODO: filter drafts based on published field
-- TODO: relativize urls

run :: Options -> Command -> IO ()
run o (Build _) = do
  options <- shakeOptions o
  shake options $ do
    buildDir <- shakeFiles <$> getShakeOptionsRules

    gitHash <- gitHashOracle

    getSite <- newCache \posts -> do
      hash <- gitHash "master"
      pure $ Site
        { author
        , email = "e.nk@caltech.edu"
        , github = "https://github.com/emekoi"
        , hash
        , lang = "en"
        , posts = List.sortOn (Ord.Down . (.published)) posts
        , source = "https://github.com/emekoi/emekoi.github.io"
        , tags = foldMap (.tags) posts
        , title = author <> "'s Blog"
        , url = "https://emekoi.github.io"
        }

    getSiteMeta <- liftAction $ Aeson.toJSON <$> getSite []

    template <- Template.compileDir "templates"

#if defined(POST_CACHE)
    renderPost <- fmap (. BuildPost) . addOracleCache $ \(BuildPost input) -> do
      Just t <- template "post.html"
      siteMeta <- getSiteMeta
      Blog.renderMarkdownIO input >>= Template.renderPost siteMeta t
#else
    renderPost <- newCache \input -> do
      Just t <- template "post.html"
      siteMeta <- getSiteMeta
      renderMarkdownIO input >>= Template.renderPost siteMeta t
#endif

    let postSlug = fmap (Text.unpack . titleSlug . (.title) . fst) . renderPost

    routeStatic "css/*.css"
    routeStatic "fonts//*"

    routePage "pages/404.md" \input output -> do
      Just t <- template "page.html"
      siteMeta <- getSiteMeta
      renderMarkdownIO input
        >>= writePage siteMeta t output

    routePage "pages/index.md" \input output -> do
      files <- getDirectoryFiles "" postsPattern
      posts <- take 5 <$> forP files (fmap fst . renderPost)

      Just tPostList <- template "post-list.md"
      Just tPage <- template "page.html"

      siteMeta <- Aeson.toJSON <$> getSite posts

      Template.preprocessFile siteMeta tPostList input
        >>= renderMarkdown input
        >>= writePage siteMeta tPage output

    routePage' "pages/posts.md" "posts/index.html" \input output -> do
      files <- getDirectoryFiles "" postsPattern
      posts <- forP files (fmap fst . renderPost)

      Just tPostList <- template "post-list.md"
      Just tPage <- template "page.html"

      siteMeta <- Aeson.toJSON <$> getSite posts

      Template.preprocessFile siteMeta tPostList input
        >>= renderMarkdown input
        >>= writePage siteMeta tPage output

    routePage' "pages/tags.md" "tags.html" \input output -> do
      files <- getDirectoryFiles "" postsPattern
      posts <- forP files (fmap fst . renderPost)

      Just tPostList <- template "post-list.md"
      Just tPage <- template "page.html"

      (tagsMeta, siteMeta) <- (tagsMeta &&& Aeson.toJSON) <$> getSite posts

      Template.preprocessFile (Aeson.Object $ "tags" .= tagsMeta) tPostList input
        >>= renderMarkdown input
        >>= writePage siteMeta tPage output

    -- build posts
    action $ do
      files <- getDirectoryFiles "" postsPattern
      forP files \file -> do
        slug <- postSlug file
        need [buildDir </> "posts" </> slug </> "index.html"]

    postInput <- liftAction do
      files <- getDirectoryFiles "" postsPattern
      Map.fromList <$> forP files \file -> do
        (post, content) <- renderPost file
        let slug = Text.unpack $ titleSlug post.title
        pure (buildDir </> "posts" </> slug </> "index.html", (file, content))

    buildDir </> "posts/*/index.html" %> \output -> do
      (input, content) <- (Map.! output) <$> postInput
      need [input]
      writeFile output (TextL.encodeUtf8 content)

  where
    author = "Emeka Nkurumeh"
    postsPattern = ["posts/*.md", "posts/*/index.md"]

run o Clean = do
  options <- shakeOptions o
  shake options . action $ do
    buildDir <- shakeFiles <$> getShakeOptions
    putInfo $ unwords ["Removing", buildDir]
    removeFilesAfter buildDir ["//*"]

parseOptions :: A.Parser Options
parseOptions = do
  command <- A.hsubparser (mconcat
    [ A.command "build" (A.info pBuild (A.progDesc "Build the site"))
    , A.command "clean" (A.info (pure Clean) (A.progDesc "Remove build files"))
    ]) <|> pBuild

  verbosity <- A.option A.auto (mconcat
    [ A.long "verbosity"
    , A.short 'v'
    , A.metavar "VERBOSITY"
    , A.completeWith $ map show [minBound :: Verbosity .. maxBound]
    ]) <|> pure Error

  jobs <- A.option A.auto (mconcat
    [ A.long "jobs"
    , A.short 'j'
    , A.metavar "N"
    ]) <|> pure (numCapabilities `div` 2)

  pure Options{..}

  where
    pBuild = fmap Build $ BuildOptions
      <$> A.switch (A.long "drafts" <> A.short 'd')

main :: IO ()
main = do
  options <- A.execParser (A.info (parseOptions A.<**> A.helper) mempty)
  run options options.command

