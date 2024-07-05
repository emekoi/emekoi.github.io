{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Blog (main) where

import           Blog.Shake
import qualified Blog.Template              as Template
import           Blog.Type
import           Blog.Util
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as Aeson
import qualified Data.List                  as List
import qualified Data.Map.Strict            as Map
import qualified Data.Ord                   as Ord
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy.Encoding    as TextL
import           Development.Shake          hiding (shakeOptions)
import qualified Development.Shake          as Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath ((</>))
import           GHC.Conc                   (numCapabilities)
import qualified Options.Applicative        as A
import           Prelude                    hiding (writeFile)

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

