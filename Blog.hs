{-# LANGUAGE NoOverloadedLists #-}

module Blog (main) where

import qualified Blog.MMark                 as Blog
import qualified Blog.Shake                 as Blog
import qualified Blog.Template              as Blog
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text.Lazy.Encoding    as TextL
import           Development.Shake          hiding (shakeOptions)
import qualified Development.Shake          as Shake
import           Development.Shake.FilePath ((</>))
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

staticFiles :: Action ()
staticFiles = do
  files <- getDirectoryFiles "." ["css/*.css", "fonts//*"]
  buildDir <- shakeFiles <$> getShakeOptions
  void $ forP files $ \file -> do
    copyFileChanged file (buildDir </> file)



run :: Options -> Command -> IO ()
run o (Build _) = do
  options <- shakeOptions o
  shake options $ do
    sequence_ [ Blog.gitHashOracle ]
    action staticFiles

    buildDir <- shakeFiles <$> getShakeOptionsRules

    template <- Blog.compileTemplates "templates"

    want [buildDir </> "404.html"]

    (buildDir </> "404.html") %> \out -> do
      Just t <- template "page.html"
      -- let file = FP.dropDirectory1 $ out -<.> "md"
      need ["pages/404.md"]
      page <- Blog.renderMarkdownIO "pages/404.md"
      output <- Blog.renderPage t page
      writeFile out (TextL.encodeUtf8 output)

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

