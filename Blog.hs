{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Blog (main) where

import           Blog.Config
import qualified Blog.MMark                     as MMark
import           Blog.Shake
import qualified Blog.Template                  as Template
import           Blog.Type
import           Blog.Util
import           Control.Applicative
import           Control.Arrow
import qualified Control.Concurrent.MVar        as MVar
import           Control.Exception
import           Control.Monad
import           Data.Aeson                     ((.=))
import qualified Data.Aeson                     as Aeson
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import           Data.String                    (fromString)
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import qualified Data.Time.Clock                as Clock
import qualified Data.Time.Format.ISO8601       as Clock
import           Development.Shake              hiding (shakeOptions)
import qualified Development.Shake              as Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath     ((</>))
import qualified Development.Shake.FilePath     as Shake
import           GHC.Clock
import           GHC.Conc                       (numCapabilities)
import           Numeric                        (showFFloat)
import qualified Options.Applicative            as A
import           Prelude                        hiding (writeFile)

#if defined(ENABLE_WATCH)
import           Data.Function                  (fix)
import           Data.Maybe                     (catMaybes)
import qualified Development.Shake.Database     as Shake
import           Development.Shake.FilePath     ((<.>))
import           GHC.Conc                       (forkIO, threadDelay)
import qualified Network.HTTP.Types.Status      as Status
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified System.Directory               as Dir
import qualified System.FSNotify                as FS
import           System.FSNotify                (Event (..))
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

data CleanOptions = CleanOptions
  { cache :: Bool
  }

data PreviewOptions = PreviewOptions
  { server :: Bool
  , host   :: String
  , port   :: Int
  }

data Command
  = Build BuildOptions
  | Clean CleanOptions
  | Preview PreviewOptions

shakeOptions :: Options -> IO ShakeOptions
shakeOptions Options{..} = do
  version <- getHashedShakeVersion ["Blog.hs"]

  pure $ Shake.shakeOptions
    { shakeColor     = True
    , shakeStaunch   = False
    , shakeThreads   = jobs
    , shakeVerbosity = verbosity
    , shakeVersion   = version
    }

-- TODO: build atom feed
-- TODO: build jsonfeed feed
-- TODO: filter drafts based on published field
-- TODO: relativize urls

postURL :: Post -> String
postURL Post{..} = "posts" </> slug </> "index.html"
  where slug = Text.unpack $ titleSlug title

tagsMeta :: Site -> Aeson.Value
tagsMeta = Aeson.toJSON . Map.foldrWithKey' f [] . tagMap
  where
    f (Tag k) v = (Aeson.object ["tag" .= ("#" <> k), "site" .= Aeson.Object ("posts" .= v)] :)
    tagMap Site{..} =
      foldr (\p posts -> Set.foldl' (\posts t -> Map.adjust (p:) t posts) posts p.tags)
      -- foldr (\p posts -> Set.foldl' (flip (Map.adjust (p :))) posts p.tags)
        (Map.fromAscList . map (, []) $ Set.toAscList tags)
        posts

build ::  BuildOptions -> Rules ()
build _ = do
  gitHash <- gitHashOracle

  buildTime <- liftIO Clock.getCurrentTime

  getSite <- newCache \posts -> do
    hash <- gitHash "master"
    pure $ siteBuild hash posts

  let getSiteMeta = Aeson.toJSON <$> getSite []

  template <- Template.compileDir "templates"

  fetchPost <- fmap (\x -> fmap (.unPostA) . x . PostQ) . addOracleCache $ \(PostQ input) -> do
    need [input]

    MMark.renderMarkdownIO postExtensions input >>= \page ->
      case Aeson.fromJSON (Aeson.Object page.meta) of
        Aeson.Error err ->
          liftIO . throwIO $ FileError (Just input) err
        Aeson.Success v | Text.null v.title ->
          liftIO . throwIO $ FileError (Just input) "missing metadata field: title"
        Aeson.Success Post{..} -> do
          (Aeson.Object meta) <- pure . Aeson.toJSON $ Post
            { tags = linkifyTags tags
            , ..
            }
          pure $ PostA
            ( Post { body = page.body, .. }
            , Page meta page.body
            )

  routeStatic "css/*.css"
  routeStatic "fonts//*"

  routePage' "resume/resume.tex" "static/resume.pdf" \input output -> do
    putInfo $ unwords ["RESUME", input]
    need ["resume/resume.sty", "resume/latexmkrc"]
    buildDir <- shakeFiles <$> getShakeOptions
    cmd_ @(String -> [String] -> _)
      "latexmk -r resume/latexmkrc -quiet"
      [ "-outdir=" ++ Shake.takeDirectory output
      , "-auxdir=" ++ buildDir
      ]

  routePage "pages/404.md" \input output -> do
    putInfo $ unwords ["PAGE", input]

    t <- template "page.html"
    siteMeta <- getSiteMeta

    renderMarkdownIO input
      >>= writePage siteMeta t output

  routePage "pages/index.md" \input output -> do
    putInfo $ unwords ["PAGE", input]

    posts <- getDirectoryFiles "" postsPattern
      >>= mapP fetchPost

    [tPostList, tPage] <- forP ["post-list.md", "page.html"] template
    siteMeta <- Aeson.toJSON <$> getSite (fst <$> take 5 posts)

    Template.preprocessFile siteMeta tPostList input
      >>= renderMarkdown input
      >>= writePage siteMeta tPage output

  routePage' "pages/posts.md" "posts/index.html" \input output -> do
    putInfo $ unwords ["PAGE", input]

    posts <- getDirectoryFiles "" postsPattern
      >>= mapP (fmap fst . fetchPost)

    [tPostList, tPage] <- forP ["post-list.md", "page.html"] template
    siteMeta <- Aeson.toJSON <$> getSite posts

    Template.preprocessFile siteMeta tPostList input
      >>= renderMarkdown input
      >>= writePage siteMeta tPage output

  routePage' "pages/tags.md" "tags.html" \input output -> do
    putInfo $ unwords ["PAGE", input]

    posts <- getDirectoryFiles "" postsPattern
      >>= mapP (fmap fst . fetchPost)

    [tPostList, tPage] <- forP ["post-list.md", "page.html"] template
    (tagsMeta, siteMeta) <- (tagsMeta &&& Aeson.toJSON) <$> getSite posts

    Template.preprocessFile (Aeson.Object $ "tags" .= tagsMeta) tPostList input
      >>= renderMarkdown input
      >>= writePage siteMeta tPage output

  routePage' "pages/feed.xml" "feed.xml" \input output -> do
    putInfo $ unwords ["FEED", input]

    let setUpdated Post{..} = Post{ updated = updated <> published, .. }
    posts <- getDirectoryFiles "" postsPattern
      >>= mapP (fmap (setUpdated . fst) . fetchPost)

    tItem <- template "atom-item.xml"

    siteMeta <- jsonInsert "updated" (fromString $ Clock.iso8601Show buildTime)
      . Aeson.toJSON <$> getSite posts

    Template.preprocessFile siteMeta tItem input
      >>= writeFile output . LBS.fromStrict . Text.encodeUtf8

    pure ()

  postsMap <- liftIO MVar.newEmptyMVar

  action $ do
    files <- getDirectoryFiles "" postsPattern
    map <- Map.fromList <$> forP files \input -> do
      (post, page) <- fetchPost input
      pure (siteOutput </> postURL post, (input, page))
    liftIO $ MVar.putMVar postsMap map
    runAfter . void $ MVar.takeMVar postsMap
    need $ Map.keys map

  siteOutput </> "posts/*/index.html" %> \output -> do
    (input, page) <- (Map.! output) <$> liftIO (MVar.readMVar postsMap)

    putInfo $ unwords ["POST", input]

    t <- template "post.html"
    siteMeta <- getSiteMeta

    writePage siteMeta t output page

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

  pure ()

  where
    postsPattern = ["posts/*.md", "posts/*/index.md"]

timerStart :: IO (IO String)
timerStart = do
  start <- getMonotonicTime
  pure $ do
    end <- getMonotonicTime
    pure . duration $ end - start
  where
    duration :: Double -> String
    duration x
        | x >= 3600 = f (x / 60) "h" "m"
        | x >= 60   = f x "m" "s"
        | otherwise = showFFloat (Just 2) x "s"

    f ((`divMod` (60 :: Int)) . round -> (ms, ss)) m s =
      show ms ++ m ++ ['0' | ss < 10] ++ show ss ++ s

#if defined(ENABLE_WATCH)
watch :: ShakeOptions -> Rules () -> IO ()
watch shakeOpts rules = do
  Shake.shakeWithDatabase shakeOpts rules \db -> FS.withManager \mgr -> do
    fix \loop -> do
      timeElapsed <- timerStart
      res <- try @ShakeException $ do
        (_, after) <- Shake.shakeRunDatabase db []
        Shake.shakeRunAfter shakeOpts after
      elapsed <- timeElapsed

      threadDelay 100000

      liveFiles <- Shake.shakeLiveFilesDatabase db >>= mapM Dir.makeAbsolute

      files <- case res of
        Right ()      -> do
          putStrLn (unwords ["Build completed in", elapsed])
          pure liveFiles
        Left shakeErr -> do
          print shakeErr
          -- NOTE: we ignore the files shake reports since they might be be
          -- something of the form 'OracleQ "file"' instead of a file path.
          errs <- Shake.shakeErrorsDatabase db
          failedDeps <- catMaybes <$> mapM (errFile . snd) errs
          pure $ failedDeps ++ liveFiles

      if null files then
        putStrLn "No files to watch"
      else do
        sema <- MVar.newEmptyMVar

        let
          watchDirs = Map.fromListWith Set.union $
            map (\file -> (Shake.takeDirectory file, Set.singleton file)) files

          startWatchers = forM (Map.toList watchDirs) \(dir, liveFilesInDir) -> do
            let isChangeToLiveFile (Modified path _ _) = path `Set.member` liveFilesInDir
                isChangeToLiveFile _                   = False
            FS.watchDir mgr dir isChangeToLiveFile \e -> do
              putStrLn $ unwords ["Change in", e.eventPath]
              MVar.putMVar sema ()

        bracket startWatchers sequence $ const do
          putStrLn "Watching for changes..."
          MVar.takeMVar sema
          putChar '\n'

        loop
  where
    errFile :: SomeException -> IO (Maybe FilePath)
    errFile err = sequence do
      shakeErr <- fromException @ShakeException err
      FileError{..} <- fromException shakeErr.shakeExceptionInner
      Dir.makeAbsolute <$> path
#endif

timedShake :: ShakeOptions -> Rules () -> IO ()
timedShake shakeOpts rules = do
  timeElapsed <- timerStart
  shake shakeOpts rules
  elapsed <- timeElapsed
  putStrLn $ unwords ["Build completed in", elapsed]

run :: Options -> Command -> IO ()
run o (Clean CleanOptions{..}) = do
  options <- shakeOptions o
  shake options . action $ do
    putInfo $ unwords ["Removing", siteOutput]
    removeFilesAfter siteOutput ["//*"]
    when cache $ do
      buildDir <- shakeFiles <$> getShakeOptions
      putInfo $ unwords ["Removing", buildDir]
      removeFilesAfter buildDir ["//*"]

#if defined(ENABLE_WATCH)
run o (Build b) = do
  shakeOpts <- shakeOptions o
  (if b.watch then watch else timedShake)
    shakeOpts
    (build b)
run o (Preview w) = do
  shakeOpts <- shakeOptions o
  _ <- forkIO (watch shakeOpts (build BuildOptions{ drafts = True, watch = True }))
  let app = Wai.staticApp (staticSettings siteOutput)
  Warp.runSettings warpSettings app
  where
    warpSettings = Warp.setHost (fromString w.host)
      $ Warp.setPort w.port Warp.defaultSettings

    nullExtension = not . Shake.hasExtension

    staticSettings path = let s = Wai.defaultFileServerSettings path in s
      { Wai.ss404Handler = Just \_ respond ->
        LBS.readFile (path </> "404.html")
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
#else
run o (Build b) | not b.watch = do
  shakeOpts <- shakeOptions o
  timedShake shakeOpts (build b)
run _ _ = error "watch/preview disabled"
#endif

parseOptions :: A.Parser Options
parseOptions = do
  command <- A.hsubparser (mconcat
    [ A.command "build" (A.info pBuild (A.progDesc "Build the site"))
    , A.command "clean" (A.info pClean (A.progDesc "Remove build files"))
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

    pClean = Clean <$> do
      cache <- A.switch (A.long "cache" <> A.short 'c' <> A.help "Clean cache")
      pure CleanOptions {..}

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
