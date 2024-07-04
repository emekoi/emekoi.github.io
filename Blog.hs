{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE TypeFamilies      #-}

module Blog (main) where

import qualified Blog.MMark                 as Blog
import           Blog.Shake
import qualified Blog.Slug                  as Blog
import qualified Blog.Template              as Blog
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as LBS
import           Data.Function              ((&))
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)
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
  -- | DynamicM (FilePath -> Action FilePath) FilePattern
  | Static Bool (Maybe FilePath) FilePath

instance IsString Route where
  fromString = Static False Nothing

getOutputRules :: FilePath -> Rules FilePath
getOutputRules file = do
  buildDir <- shakeFiles <$> getShakeOptionsRules
  pure $ buildDir </> file

route :: Route -> (FilePath -> FilePath -> Action ()) -> Rules [FilePath]
route (Static isPat input output) f = do
  output <- getOutputRules output
  unless isPat $ want [output]
  output %> \x -> f (fromMaybe x input) x
  pure [output]
route (Dynamic g pat) f = do
  buildDir <- shakeFiles <$> getShakeOptionsRules
  let getOut x = buildDir </> g x

  -- split into 2 steps to avoid indirect recursion
  action $ getDirectoryFiles "" [pat] >>= need . fmap getOut

  -- convert an Action r to a Rules (Action r)
  outputMap <- fmap ($ ()) $ newCache \() ->
    Map.fromList <$> fmap (\x -> (getOut x, x)) <$> getDirectoryFiles "" [pat]

  getOut pat %> \output -> do
    input <- (Map.! output) <$> outputMap
    need [input]
    f input output
  pure []
-- route (DynamicM g pat) f = do
--   buildDir <- shakeFiles <$> getShakeOptionsRules
--   let getOut x = (buildDir </>) <$> g x

--   -- split into 2 steps to avoid indirect recursion
--   action $ getDirectoryFiles "" [pat]
--     >>= flip forP getOut
--     >>= need

--   -- convert an Action r to a Rules (Action r)
--   outputMap <- fmap ($ ()) $ newCache \() ->
--     Map.fromList <$> fmap (\x -> (getOut x, x)) <$> getDirectoryFiles "" [pat]

--   action $ void outputMap
--   getOut pat %> \output -> do
--     input <- (Map.! output) <$> outputMap
--     f input output
--   pure []

route_ :: Route -> (FilePath -> FilePath -> Action ()) -> Rules ()
route_ r k = void $ route r k

routeStatic :: FilePattern -> Rules ()
routeStatic = flip route_ copyFileChanged . Dynamic id

routePage :: FilePath -> (FilePath -> FilePath -> Action ()) -> Rules ()
routePage = route_ . Dynamic (\input -> FP.takeBaseName input <.> "html")

-- root :: String -> (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
-- root help test act = addUserRule $ FileRule help $ \x -> if not $ test x then Nothing else Just $ ModeDirect $ do
--   Dir.createDirectoryIfMissing True (FP.takeDirectory x)
--   act x

newtype GitHash = GitHash String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult GitHash = Text

gitHashOracle :: Rules (String -> Action Text)
gitHashOracle = fmap (. GitHash) . addOracle $ \(GitHash branch) -> Text.strip . Text.decodeUtf8 . fromStdout <$>
  cmd @(String -> [String] -> Action _) "git" ["rev-parse", "--short", branch]

newtype BuildPost = BuildPost String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult BuildPost = (Post, TextL.Text, FilePath)

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
    --   (post, page) <- Blog.renderMarkdownIO input >>= Blog.renderPost site t
    --   pure (post, page, Text.unpack $ Blog.titleSlug post.title)

    renderPost <- newCache \input -> do
      Just t <- template "post.html"
      site <- getSite
      (post, page) <- Blog.renderMarkdownIO input >>= Blog.renderPost site t
      pure (post, page, Text.unpack $ Blog.titleSlug post.title)

    let postSlug = fmap (\(_, _, x) -> x) . renderPost

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
      input & (Blog.preprocess site
        >=> Blog.renderMarkdown input
        >=> Blog.renderPage site t
        >=> (writeFile output . TextL.encodeUtf8))

    action $ do
      files <- getDirectoryFiles "" ["posts/*.md"]
      forP files \file -> do
        slug <- postSlug file
        need [buildDir </> "posts" </> slug </> "index.html"]

    -- convert an Action r to a Rules (Action r)
    !outputMap <- fmap ($ ()) $ newCache \() -> do
      files <- getDirectoryFiles "" ["posts/*.md"]
      m <- Map.fromList <$> forP files \file -> do
        slug <- postSlug file
        pure (buildDir </> "posts" </> slug </> "index.html", file)
      liftIO $ print m
      pure m

    buildDir </> "posts/*/index.html" %> \output -> do
      input <- (Map.! output) <$> outputMap
      need [input]
      (_, content, _) <- renderPost input
      writeFile output (TextL.encodeUtf8 content)

    pure ()
    -- renderPost <- curry <$> newCache \(input, output) -> do
    --   Just t <- template "post.html"
    --   (post, page) <- Blog.renderMarkdownIO input >>= Blog.renderPost t
    --   writeFile output (TextL.encodeUtf8 page)
    --   pure post

    -- posturl <- newCache (pure . Blog.titleSlug)

  where
    -- _3 :: (a, b, c) -> c
    -- _3 (_, _, c) = c

    author = "Emeka Nkurumeh"

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

