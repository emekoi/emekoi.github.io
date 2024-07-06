{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Blog (main) where

import           Blog.Shake
import qualified Blog.Template                  as Template
import           Blog.Type
import           Blog.Util
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Aeson                     ((.=))
import qualified Data.Aeson                     as Aeson
import qualified Data.List                      as List
import qualified Data.Map.Strict                as Map
import qualified Data.Ord                       as Ord
import qualified Data.Text                      as Text
import qualified Data.Text.Lazy.Encoding        as TextL
import           Development.Shake              hiding (shakeOptions)
import qualified Development.Shake              as Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath     ((</>))
import qualified Development.Shake.FilePath     as Shake
import           GHC.Conc                       (numCapabilities)
import qualified Options.Applicative            as A
import           Prelude                        hiding (writeFile)

#if defined(PREVIEW_SERVER)
import qualified Data.ByteString.Lazy.Char8     as BS
import           Data.String                    (fromString)
import           Development.Shake.FilePath     ((<.>))
import qualified Network.HTTP.Types.Status      as Status
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified System.Directory               as Dir
import qualified WaiAppStatic.Types             as Wai
#endif

data Options = Options
  { command   :: Command
  , verbosity :: Verbosity
  , jobs      :: Int
  }

newtype BuildOptions = BuildOptions
  { _buildDrafts :: Bool
  }

data WatchOptions = WatchOptions
  { server :: Bool
  , host   :: String
  , port   :: Int
  }

data Command
  = Build BuildOptions
  | Clean
  | Watch WatchOptions

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

#if defined(PREVIEW_SERVER)
staticSettings :: FilePath -> Wai.StaticSettings
staticSettings path = let s = Wai.defaultFileServerSettings path in s
  { Wai.ss404Handler = Just \_ respond ->
    BS.readFile (path </> "404.html")
      >>= respond . Wai.responseLBS Status.status404 []
  , Wai.ssLookupFile = \pieces ->
    case splitAt (length pieces - 1) pieces of
      (prefix, [Wai.fromPiece -> fileName])
        | nullExtension (Text.unpack fileName) ->
          s.ssLookupFile $ prefix <> [Wai.unsafeToPiece $ fileName <> ".html"]
      _ -> s.ssLookupFile pieces
  , Wai.ssGetMimeType = \file ->
    let fileName = Text.unpack $ Wai.fromPiece file.fileName in
    if nullExtension fileName then do
      htmlExists <- Dir.doesFileExist $ path </> fileName <.> "html"
      if htmlExists then pure "text/html" else s.ssGetMimeType file
    else s.ssGetMimeType file
  }
  where
    nullExtension :: FilePath -> Bool
    nullExtension = not . Shake.hasExtension
#endif

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
        , description = author <> "'s Blog"
        , email       = "e.nk@caltech.edu"
        , github      = "https://github.com/emekoi"
        , hash
        , lang        = "en"
        , posts       = List.sortOn (Ord.Down . (.published)) posts
        , source      = "https://github.com/emekoi/emekoi.github.io"
        , tags        = foldMap (.tags) posts
        , title       = author <> "'s Blog"
        , url         = "https://emekoi.github.io"
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
    postInput <- liftAction do
      files <- getDirectoryFiles "" postsPattern
      Map.fromList <$> forP files \file -> do
        (post, content) <- renderPost file
        let slug = Text.unpack $ titleSlug post.title
        pure (buildDir </> "posts" </> slug </> "index.html", (file, content))

    buildDir </> "posts/*/index.html" %> \output -> do
      (input, content) <- (Map.! output) <$> postInput
      writeFile output (TextL.encodeUtf8 content)

      let
        outDir = Shake.takeDirectory output
        inDir  = Shake.takeDirectory input

      deps <- if length (Shake.splitPath inDir) == 1
        then pure [input]
        else do
          fmap (inDir </>) <$> getDirectoryContents inDir

      forP deps \file -> do
        copyFileChanged file (Shake.replaceDirectory file outDir)

      need deps

    action $ do
      files <- getDirectoryFiles "" postsPattern
      forP files \file -> do
        slug <- postSlug file
        need [buildDir </> "posts" </> slug </> "index.html"]

      pure ()

  where
    author = "Emeka Nkurumeh"
    postsPattern = ["posts/*.md", "posts/*/index.md"]

#if defined(PREVIEW_SERVER)
run _ (Watch w) = do
  let app = Wai.staticApp (staticSettings "_build")
  Warp.runSettings warpSettings app
  where
    warpSettings = Warp.setHost (fromString w.host)
      $ Warp.setPort w.port Warp.defaultSettings
#else
run _ (Watch w) = error "watch disabled"
#endif

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
    , A.command "watch" (A.info pWatch (A.progDesc "Watch for changed and rebuild automatically"))
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
      <$> A.switch (A.long "drafts" <> A.short 'd' <> A.help "Include drafts")
    pWatch = Watch <$> do
      server <- A.switch (A.long "server" <> A.short 's' <> A.help "Run a preview server")
      host <- A.option A.auto (mconcat
        [ A.long "host"
        , A.short 'h'
        , A.metavar "HOST"
        , A.value "127.0.0.1"
        , A.showDefault
        ])
      port <- A.option A.auto (mconcat
        [ A.long "port"
        , A.short 'p'
        , A.metavar "PORT"
        , A.value 8000
        , A.showDefault
        ])
      pure WatchOptions {..}

main :: IO ()
main = do
  options <- A.execParser (A.info (parseOptions A.<**> A.helper) mempty)
  run options options.command
