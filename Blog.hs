{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE TypeFamilies      #-}

module Blog
    ( main
    ) where

import Blog.Agda                      qualified as Agda
import Blog.Config
import Blog.MMark                     qualified as MMark
import Blog.Shake
import Blog.Template                  qualified as Template
import Blog.Type
import Blog.Util
import Control.Applicative
import Control.Arrow
import Control.Concurrent.MVar        qualified as MVar
import Control.Monad
import Data.Aeson                     ((.=))
import Data.Aeson                     qualified as Aeson
import Data.ByteString.Lazy           qualified as LBS
import Data.List                      qualified as List
import Data.Map.Strict                qualified as Map
import Data.Set                       qualified as Set
import Data.Text                      qualified as Text
import Data.Text.Encoding             qualified as Text
import Data.Text.IO                   qualified as Text
import Data.Time.Clock                qualified as Clock
import Data.Time.Format.ISO8601       qualified as Clock
import Development.Shake              hiding (shakeOptions)
import Development.Shake              qualified as Shake
import Development.Shake.Classes
import Development.Shake.FilePath     ((<.>), (</>))
import Development.Shake.FilePath     qualified as Shake
import GHC.Clock
import GHC.Conc                       (numCapabilities)
import Numeric                        (showFFloat)
import Options.Applicative            qualified as A
import Prelude                        hiding (writeFile)

#if defined(ENABLE_WATCH)
import Control.Exception
import Data.Function                  (fix)
import Data.Maybe                     (catMaybes)
import Data.String                    (fromString)
import Development.Shake.Database     qualified as Shake
import GHC.Conc                       (forkIO, threadDelay)
import Network.HTTP.Types.Status      qualified as Status
import Network.Wai                    qualified as Wai
import Network.Wai.Application.Static qualified as Wai
import Network.Wai.Handler.Warp       qualified as Warp
import System.Directory               qualified as Dir
import System.FSNotify                (Event (..))
import System.FSNotify                qualified as FS
import WaiAppStatic.Types             qualified as Wai
#endif

data Options = Options
  { command   :: Command
  , verbosity :: Verbosity
  , jobs      :: Int
  }

data BuildOptions = BuildOptions
  { watch :: Bool
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

-- TODO: filter drafts based on published field
-- TODO: relativize urls

postURL :: Post -> String
postURL Post{..} = "posts" </> slug </> "index.html"
  where slug = Text.unpack $ titleSlug title

pubList :: FilePath -> Action [Aeson.Value]
pubList file = do
  need [file]
  liftIO (Aeson.decode <$> LBS.readFile file) >>= \case
    Nothing   -> fileError (Just file) "invalid json"
    Just works -> forM works \Publication{..} ->
      pure $ Aeson.object
        [ "title"   .= title
        , "uri"     .= uri
        , "authors" .= mconcat (List.intersperse ", " authors)
        ]

atomFeedItem :: Post -> Post
atomFeedItem Post{..} = Post
  { updated = updated <> published
  , ..
  }

jsonFeed :: Site -> Aeson.Value
jsonFeed Site{..} = Aeson.object
  [ "version"       .= ("https://jsonfeed.org/version/1.1" :: String)
  , "title"         .= title
  , "home_page_url" .= url
  , "feed_url"      .= (url <> "/" <> "feed.json")
  , "description"   .= description
  , "authors"       .=
    [ Aeson.object
      [ "name" .= author
      , "url"  .= url
      ]
    ]
  , "language"      .= lang
  , "items"         .= (jsonFeedItem <$> posts)
  ]
  where
    jsonFeedItem :: Post -> Aeson.Value
    jsonFeedItem Post{..} = Aeson.object
      [ "id"             .= (url <> "/" <> slug)
      , "url"            .= (url <> "/" <> slug)
      , "content_html"   .= body
      , "title"          .= title
      , "date_published" .= published.iso8601
      , "date_modified"  .= (updated.iso8601 <|> published.iso8601)
      , "tags"           .= tags
      ]

tagsMeta :: Site -> Aeson.Value
tagsMeta = Aeson.toJSON . Map.foldrWithKey' f [] . tagMap
  where
    f (Tag k) v = (Aeson.object ["tag" .= ("#" <> k), "site" .= Aeson.Object ("posts" .= v)] :)
    tagMap Site{..} =
      foldr (\p posts -> Set.foldl' (flip (Map.adjust (p :))) posts p.tags)
        (Map.fromAscList . map (, []) $ Set.toAscList tags)
        posts

build ::  BuildOptions -> Rules ()
build b = do
  gitHash <- gitHashOracle

  buildTime <- liftIO Clock.getCurrentTime

  getSite <- newCache \posts -> do
    hash <- gitHash "master"
    pure $ siteBuild hash posts

  let getSiteMeta = Aeson.toJSON <$> getSite []

  template <- Template.compileDir "templates"

  -- fetch and render posts, wrapping the underlying oracle
  fetchPost <- fmap wrapPostQ . addOracleCache $ \(PostQ input) -> do
    need [input]

    (source, input) <- case Shake.splitExtensions input of
      (Shake.takeBaseName -> base, ".lagda.md") ->
        let input = "agda" </> base <.> "md" in
        need [input] *> liftIO (Text.readFile input)
          >>= fmap (,input) . Agda.readAgda input
      _ -> (, input) <$> liftIO (Text.readFile input)

    MMark.renderMarkdown postExtensions input source >>= \page ->
      case Aeson.fromJSON (Aeson.Object page.meta) of
        Aeson.Error err ->
          fileError (Just input) err
        Aeson.Success v | Text.null v.title ->
          fileError (Just input) "missing metadata field: title"
        Aeson.Success Post{..} -> do
          (Aeson.Object meta) <- pure . Aeson.toJSON $ Post
            { tags = linkifyTags tags
            , ..
            }
          pure $ PostA
            ( Post { body = page.body, .. }
            , Page meta page.body
            )

  -- copy static resources
  staticFiles "css/*.css"
  staticFiles "fonts//*"
  staticFiles "static/*"

  -- copy static agda files
  route (Dynamic (`Shake.replaceDirectory` "static/agda") "agda/*.html" ) \input output -> do
    putInfo $ unwords ["AGDA", output]

    let mkPage = Page ("title" .= Shake.takeBaseName input)

    tPage <- template "agda-page.html"
    siteMeta <- getSiteMeta

    liftIO (Text.readFile input)
      >>= Agda.readAgdaNaked input
      >>= (writePage siteMeta tPage output . mkPage)

  -- compile literate agda files
  adgaSrcMap <- liftAction $
    Map.fromList . fmap (\x -> (agdaOut x, x)) <$> getDirectoryFiles "" ["posts/*.lagda.md"]

  agdaOut "posts/*.lagda.md" %> \output -> do
    putInfo $ unwords ["AGDA", output]
    input <- (Map.! output) <$> adgaSrcMap
    need [input]
    command_ [Cwd "posts"] "agda"
      [ "--html"
      , "--html-highlight=code"
      , "--html-dir=../agda"
      , "--css=agda.css"
      , Shake.takeFileName input
      ]

  -- build resume
  routeStatic "resume/resume.tex" "static/resume.pdf" \_ output -> do
    putInfo $ unwords ["RESUME", output]

    need ["resume/resume.sty", "resume/latexmkrc"]
    buildDir <- shakeFiles <$> getShakeOptions
    cmd_ @(String -> [String] -> _)
      "latexmk -r resume/latexmkrc -quiet"
      [ "-outdir=" ++ Shake.takeDirectory output
      , "-auxdir=" ++ buildDir
      ]

  -- write JSON feed
  routeStatic1 "feed.json" \output -> do
    putInfo $ unwords ["FEED", output]

    feed <- getPostFiles
      >>= mapP (fmap fst . fetchPost)
      >>= fmap jsonFeed . getSite

    liftIO $ Aeson.encodeFile output feed

  -- write atom feed
  routeStatic "pages/feed.xml" "feed.xml" \input output -> do
    putInfo $ unwords ["FEED", output]
    tItem <- template "atom-item.xml"

    posts <- getPostFiles
      >>= mapP (fmap (atomFeedItem . fst) . fetchPost)

    siteMeta <- jsonInsert "updated" (Clock.iso8601Show buildTime)
      . Aeson.toJSON <$> getSite posts

    Template.preprocessFile siteMeta tItem input
      >>= writeFile output . LBS.fromStrict . Text.encodeUtf8

  -- index page
  routePage "pages/index.md" \input output -> do
    putInfo $ unwords ["PAGE", output]

    posts <- getPostFiles
      >>= mapP fetchPost

    works <- pubList "pages/publications.json"

    [tPostList, tPage] <- forP ["post-list.md", "page.html"] template
    siteMeta <- jsonInsert "publications" works
      . Aeson.toJSON <$> getSite (fst <$> take 5 posts)

    Template.preprocessFile siteMeta tPostList input
      >>= renderMarkdown input
      >>= writePage siteMeta tPage output

  -- post archive
  routeStatic "pages/posts.md" "posts/index.html" \input output -> do
    putInfo $ unwords ["PAGE", output]

    posts <- getPostFiles
      >>= mapP (fmap fst . fetchPost)

    [tPostList, tPage] <- forP ["post-list.md", "page.html"] template
    siteMeta <- Aeson.toJSON <$> getSite posts

    Template.preprocessFile siteMeta tPostList input
      >>= renderMarkdown input
      >>= writePage siteMeta tPage output

  -- post/tag mappings
  routeStatic "pages/tags.md" "tags.html" \input output -> do
    putInfo $ unwords ["PAGE", output]

    posts <- getPostFiles
      >>= mapP (fmap fst . fetchPost)

    [tPostList, tPage] <- forP ["post-list.md", "page.html"] template
    (tagsMeta, siteMeta) <- (tagsMeta &&& Aeson.toJSON) <$> getSite posts

    Template.preprocessFile (Aeson.Object $ "tags" .= tagsMeta) tPostList input
      >>= renderMarkdown input
      >>= writePage siteMeta tPage output

  -- notes page
  routeStatic "pages/notes.md" "notes.html" \input output -> do
    putInfo $ unwords ["PAGE", output]

    tPage <- template "page.html"
    siteMeta <- getSiteMeta

    MMark.renderMarkdownIO (noteEntry : defaultExtensions) input
      >>= writePage siteMeta tPage output

  -- 404 page
  routePage "pages/404.md" \input output -> do
    putInfo $ unwords ["PAGE", output]

    t <- template "page.html"
    siteMeta <- getSiteMeta

    renderMarkdownIO input
      >>= writePage siteMeta t output

  postsMap <- liftIO MVar.newEmptyMVar

  -- need all posts and map outputs to inputs
  action $ do
    files <- getDirectoryFiles "" postPattern
    map <- Map.fromList <$> forP files \input -> do
      (post, page) <- fetchPost input
      pure (siteOutput </> postURL post, (input, page))
    liftIO $ MVar.putMVar postsMap map
    runAfter . void $ MVar.takeMVar postsMap
    need $ Map.keys map

  -- render each post
  siteOutput </> "posts/*/index.html" %> \output -> do
    putInfo $ unwords ["POST", output]
    (input, page) <- (Map.! output) <$> liftIO (MVar.readMVar postsMap)
    t <- template "post.html"
    siteMeta <- getSiteMeta

    writePage siteMeta t output page

    let
      outDir = Shake.takeDirectory output
      inDir  = Shake.takeDirectory input

    -- copy over associated static data
    deps <- if length (Shake.splitPath inDir) == 1
      then pure [input]
      else do
        fmap (inDir </>) <$> getDirectoryContents inDir

    forP deps \file -> do
      copyFileChanged file (Shake.replaceDirectory file outDir)

    need deps

  where
    postFolders
      |  b.watch  = ["drafts", "posts"]
      | otherwise = ["posts"]
    postExts = ["md", "lhs"]

    postPattern = concat [
      [ dir </> "*" <.> ext
      , dir </> "*" </> "index" <.> ext
      , dir </> "*" </> "Index" <.> ext
      ] | dir <- postFolders
        , ext <- postExts
      ]

    agdaOut x = Shake.replaceDirectory (Shake.replaceExtensions x "md") "agda"

    getPostFiles = getDirectoryFiles "" postPattern

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
  _ <- forkIO (watch shakeOpts (build BuildOptions{ watch = True }))
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
        , A.value 8080
        , A.showDefault
        ])
      pure PreviewOptions {..}

main :: IO ()
main = do
  options <- A.execParser (A.info (parseOptions A.<**> A.helper) mempty)
  run options options.command
