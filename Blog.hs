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

#if defined(ENABLE_WATCH)
import qualified Control.Concurrent.MVar        as MVar
import           Control.Exception
import qualified Data.ByteString.Lazy.Char8     as BS
import           Data.Function                  (fix)
import           Data.Functor
import qualified Data.Set                       as Set
import           Data.String                    (fromString)
import qualified Development.Shake.Database     as Shake
import           Development.Shake.FilePath     ((<.>))
import           GHC.Clock
import           GHC.Conc                       (forkIO, threadDelay)
import qualified Network.HTTP.Types.Status      as Status
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified System.Directory               as Dir
import           System.FSNotify
import qualified WaiAppStatic.Types             as Wai
#endif

data Options = Options
  { command   :: Command
  , verbosity :: Verbosity
  , jobs      :: Int
  }

data BuildOptions = BuildOptions
  { drafts :: Bool
  , watch  :: Bool
  }

data PreviewOptions = PreviewOptions
  { server :: Bool
  , host   :: String
  , port   :: Int
  }

data Command
  = Build BuildOptions
  | Clean
  | Preview PreviewOptions

shakeOptions :: Options -> IO ShakeOptions
shakeOptions Options{..} = do
  version <- getHashedShakeVersion ["Blog.hs"]

  pure $ Shake.shakeOptions
    { shakeColor     = True
    , shakeFiles     = "_build"
    , shakeStaunch   = False
    , shakeThreads   = jobs
    , shakeVerbosity = verbosity
    , shakeVersion   = version
    }

#if defined(ENABLE_WATCH)
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

build ::  BuildOptions -> Rules ()
build _ = do
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

  let getSiteMeta = Aeson.toJSON <$> getSite []
  -- getSiteMeta <- liftAction $ Aeson.toJSON <$> getSite []

  template <- Template.compileDir "templates"

#if defined(POST_CACHE)
  renderPost <- fmap (. BuildPost) . addOracleCache $ \(BuildPost input) -> do
    Just t <- template "post.html"
    siteMeta <- getSiteMeta
    Blog.renderMarkdownIO input >>= Template.renderPost siteMeta t
#else
  renderPost <- pure \input -> do
    putInfo ("reading " ++ input)
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
    posts <- forP files (fmap fst . renderPost)

    Just tPostList <- template "post-list.md"
    Just tPage <- template "page.html"

    siteMeta <- Aeson.toJSON <$> getSite (take 5 posts)

    Template.preprocessFile siteMeta tPostList input
      >>= renderMarkdown input
      >>= writePage siteMeta tPage output

    forP_ files \file -> do
      slug <- postSlug file
      need [buildDir </> "posts" </> slug </> "index.html"]

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
    (input, _) <- (Map.! output) <$> postInput
    (post, content) <- renderPost input

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

  -- action $ do
  --   files <- getDirectoryFiles "" postsPattern
  --   forP files \file -> do
  --     slug <- postSlug file
  --     need [buildDir </> "posts" </> slug </> "index.html"]

  --   pure ()

  where
    author = "Emeka Nkurumeh"
    postsPattern = ["posts/*.md", "posts/*/index.md"]

timerStart :: IO (IO Double)
timerStart = do
  start <- getMonotonicTime
  pure $ do
    end <- getMonotonicTime
    pure (end - start)

{- CURRENT ISSUES
- using caching means that we end up using stale results when running the rules multiple times in watch mode
- since we need all the files through a top level rule, if any of the files are bad, then we end up failing
  super early so we don't have a record of real targets that failed
- when we fail the target isn't in the list of live files so we can't wait for it to be updated again
-}

watch :: ShakeOptions -> Rules () -> IO ()
watch shakeOpts rules = do
  Shake.shakeWithDatabase shakeOpts rules \db -> withManager \mgr -> do
    fix \loop -> do
      -- oldLiveFiles <- Shake.shakeLiveFilesDatabase db >>= mapM Dir.makeAbsolute

      timeElapsed <- timerStart
      res <- try @ShakeException $ do
        (_, after) <- Shake.shakeRunDatabase db []
        Shake.shakeRunAfter shakeOpts after
      elapsed <- timeElapsed

      threadDelay 100000

      errFile <- case res of
        Right () ->
          putStrLn (unwords ["Build completed in", show elapsed]) $> Nothing
        Left shakeErr -> do
          print shakeErr

          sequence do
            FileError{..} <- fromException shakeErr.shakeExceptionInner
            Dir.makeAbsolute <$> path

      liveFiles <- Shake.shakeLiveFilesDatabase db >>= mapM Dir.makeAbsolute
      failedFiles <- do
        -- TODO: collect FileErrors from these as well
        allErrors <- Shake.shakeErrorsDatabase db >>= mapM (Dir.makeAbsolute . fst)
        pure $ maybe allErrors (: allErrors) errFile

      if null liveFiles then
        putStrLn "No live files"
      else do
        sema <- MVar.newEmptyMVar

        let
          files = failedFiles ++ liveFiles
          liveDirs = Map.fromListWith Set.union $
            map (\file -> (Shake.takeDirectory file, Set.singleton file)) files

          startWatchers = forM (Map.toList liveDirs) \(dir, liveFilesInDir) -> do
            let isChangeToLiveFile (Modified path _ _) = path `Set.member` liveFilesInDir
                isChangeToLiveFile _                   = False
            watchDir mgr dir isChangeToLiveFile \e -> do
              putStrLn $ unwords ["Change in", e.eventPath]
              MVar.putMVar sema ()

        bracket startWatchers sequence $ const do
          putStrLn "Watching for changes..."
          MVar.takeMVar sema
          putChar '\n'

        loop

timedShake :: ShakeOptions -> Rules () -> IO ()
timedShake shakeOpts rules = do
  timeElapsed <- timerStart
  shake shakeOpts rules
  elapsed <- timeElapsed
  putStrLn $ unwords ["Build completed in", show elapsed]

run :: Options -> Command -> IO ()
run o (Build b) = do
  shakeOpts <- shakeOptions o
  (if b.watch then watch else timedShake)
    shakeOpts
    (build b)

#if defined(ENABLE_WATCH)
run o (Preview w) = do
  shakeOpts <- shakeOptions o
  _ <- forkIO (watch shakeOpts (build BuildOptions{ drafts = True, watch = True }))
  let app = Wai.staticApp (staticSettings shakeOpts.shakeFiles)
  Warp.runSettings warpSettings app
  where
    warpSettings = Warp.setHost (fromString w.host)
      $ Warp.setPort w.port Warp.defaultSettings
#else
run _ (Preview e) = error "preview disabled"
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
    , A.command "preview" (A.info pPreview (A.progDesc "Run a preview server"))
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
    pBuild = Build <$> do
      drafts <- A.switch (A.long "drafts" <> A.short 'd' <> A.help "Include drafts")
      watch <- A.switch (A.long "watch" <> A.short 'w' <> A.help "Watch for changes and rebuild automatically")
      pure BuildOptions {..}

    pPreview = Preview <$> do
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
      pure PreviewOptions {..}

main :: IO ()
main = do
  options <- A.execParser (A.info (parseOptions A.<**> A.helper) mempty)
  run options options.command
