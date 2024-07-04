{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}

module Blog (main) where

import qualified Blog.MMark                 as Blog
import           Blog.Shake
import qualified Blog.Template              as Blog
import           Blog.Util
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
-- import           Data.Aeson                 (Value (..), (.=))
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as LBS
import           Data.Function              ((&))
import qualified Data.Map.Strict            as Map
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

data BuildOptions = BuildOptions
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

data Route
  = Dynamic (FilePath -> FilePath) FilePattern
  -- | DynamicM (FilePath -> Action FilePath) FilePattern FilePattern
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
    _ -> f x x
route (Dynamic g pat) f = do
  buildDir <- shakeFiles <$> getShakeOptionsRules
  let getOut x = buildDir </> g x

  -- split into 2 steps to avoid indirect recursion
  action $ getDirectoryFiles "" [pat] >>= need . fmap getOut

  outputMap <- liftAction $
    Map.fromList <$> fmap (\x -> (getOut x, x)) <$> getDirectoryFiles "" [pat]

  getOut pat %> \output -> do
    input <- (Map.! output) <$> outputMap
    need [input]
    f input output

-- route (DynamicM g outputs inputs) f = do
--   buildDir <- shakeFiles <$> getShakeOptionsRules
--   let getOut x = (buildDir </>) <$> g x

--   -- split into 2 steps to avoid indirect recursion
--   action $ getDirectoryFiles "" [inputs]
--     >>= flip forP getOut
--     >>= need

--   -- convert an Action r to a Rules (Action r)
--   outputMap <- fmap ($ ()) $ newCache \() -> do
--     files <- getDirectoryFiles "" [inputs]

--     Map.fromList <$> forP files \input -> do
--       output <- getOut input
--       pure (output, input)

--   outputs %> \output -> do
--     input <- (Map.! output) <$> outputMap
--     f input output
--   pure []

routeStatic :: FilePattern -> Rules ()
routeStatic = flip route copyFileChanged . Dynamic id

routePage :: FilePath -> (FilePath -> FilePath -> Action ()) -> Rules ()
routePage = route . Dynamic (\input -> FP.takeBaseName input <.> "html")

routePage' :: FilePath -> FilePath -> (FilePath -> FilePath -> Action ()) -> Rules ()
routePage' input output = route (Static False (Just input) output)

newtype GitHash = GitHash String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult GitHash = Text

gitHashOracle :: Rules (String -> Action Text)
gitHashOracle = fmap (. GitHash) . addOracle $ \(GitHash branch) -> Text.strip . Text.decodeUtf8 . fromStdout <$>
  cmd @(String -> [String] -> Action _) "git" ["rev-parse", "--short", branch]

newtype BuildPost = BuildPost String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult BuildPost = (Post, TextL.Text)

run :: Options -> Command -> IO ()
run o (Build _) = do
  options <- shakeOptions o
  shake options $ do
    buildDir <- shakeFiles <$> getShakeOptionsRules

    gitHash <- gitHashOracle

    getSite <- fmap ($ ()) $ newCache \() -> do
      hash <- gitHash "master"
      pure $ Aeson.toJSON  Site
        { author
        , email = "e.nk@caltech.edu"
        , github = "https://github.com/emekoi"
        , hash
        , lang = "en"
        , source = "https://github.com/emekoi/emekoi.github.io"
        , title = author <> "'s Blog"
        , url = "https://emekoi.github.io"
        }

    template <- Blog.compileTemplates "templates"

    -- renderPost <- fmap (. BuildPost) . addOracleCache $ \(BuildPost input) -> do
    --   Just t <- template "post.html"
    --   site <- getSite
    --   Blog.renderMarkdownIO input >>= Blog.renderPost site t

    renderPost <- newCache \input -> do
      Just t <- template "post.html"
      site <- getSite
      Blog.renderMarkdownIO input >>= Blog.renderPost site t

    let postSlug = fmap (Text.unpack . titleSlug . (.title) . fst) . renderPost

    routeStatic "css/*.css"
    routeStatic "fonts//*"

    routePage "pages/404.md" \input output -> do
      Just t <- template "page.html"
      site <- getSite
      input & (Blog.renderMarkdownIO
        >=> Blog.renderPage site t
        >=> (writeFile output . TextL.encodeUtf8))

    routePage "pages/index.md" \input output -> do
      Just t <- template "page.html"
      site <- getSite
      input & (Blog.preprocessFile site t
        >=> Blog.renderMarkdown input
        >=> Blog.renderPage site t
        >=> (writeFile output . TextL.encodeUtf8))

    routePage' "pages/posts.md" "posts/index.html" \input output -> do
      files <- getDirectoryFiles "" ["posts/*.md"]
      postList <- makePostList <$> forP files (fmap fst . renderPost)

      Just tPostList <- template "post-list.md"
      Just tPage <- template "page.html"
      site <- getSite

      input & (Blog.preprocessFile (Aeson.toJSON postList) tPostList
        >=> Blog.renderMarkdown input
        >=> Blog.renderPage site tPage
        >=> (writeFile output . TextL.encodeUtf8))

    -- routePage' "pages/posts.md" "posts/index.html" \input output -> do
    --   files <- getDirectoryFiles "" ["posts/*.md"]
    --   postList <- makePostList <$> forP files (fmap fst . renderPost)

    --   Just t <- template "post-list.md"
    --   -- site <- getSite

    --   -- raw <- Blog.preprocessFile (Aeson.toJSON postList) "templates/post-list.md.mustache"
    --   body <- Blog.preprocess (Aeson.toJSON postList) t "{{> post-list.md }}"
    --   let page = "---\ntitle: Posts\n---\n" <> body
    --   -- liftIO $ print (Aeson.toJSON postList)
    --   -- Blog.renderPage site t content >>= (writeFile output . TextL.encodeUtf8)
    --   liftIO $ Text.writeFile output body

    --   -- content <- Blog.renderMarkdown "" page
    --   -- Blog.renderPage site t content >>= (writeFile output . TextL.encodeUtf8)
    --   pure ()
    --   --   >=> Blog.renderPage site t
    --   --   >=> (writeFile output . TextL.encodeUtf8))

    -- build posts
    action $ do
      files <- getDirectoryFiles "" ["posts/*.md"]
      forP files \file -> do
        slug <- postSlug file
        need [buildDir </> "posts" </> slug </> "index.html"]

    -- convert an Action r to a Rules (Action r)
    postInput <- liftAction do
      files <- getDirectoryFiles "" ["posts/*.md"]
      Map.fromList <$> forP files \file -> do
        (post, content) <- renderPost file
        let slug = Text.unpack $ titleSlug post.title
        pure (buildDir </> "posts" </> slug </> "index.html", (file, content))

    buildDir </> "posts/*/index.html" %> \output -> do
      (input, content) <- (Map.! output) <$> postInput
      need [input]
      writeFile output (TextL.encodeUtf8 content)

    pure ()

  where author = "Emeka Nkurumeh"

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

  verbosity <- (A.option A.auto $ mconcat
    [ A.long "verbosity"
    , A.short 'v'
    , A.metavar "VERBOSITY"
    , A.completeWith $ map show [minBound :: Verbosity .. maxBound]
    ]) <|> pure Error

  jobs <- (A.option A.auto $ mconcat
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

