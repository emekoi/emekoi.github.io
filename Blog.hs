{-# LANGUAGE DeriveGeneric #-}

module Blog where

import           Control.Applicative
import           Development.Shake
import           GHC.Conc            (numCapabilities)
import qualified Options.Applicative as A

data Options = Options
  { command   :: Command
  , verbosity :: Verbosity
  , jobs      :: Int
  }

data BuildOptions = BuildOptions
  { buildDrafts :: Bool
  }

data WatchOptions = WatchOptions
  { port :: Int
  }

data Command
  = Watch WatchOptions
  | Build BuildOptions

-- copyStaticFiles :: Action ()
-- copyStaticFiles = do
--     filepaths <- getDirectoryFiles "./site/" ["images//*", "css//*", "js//*"]
--     void $ forP filepaths $ \filepath ->
--         copyFileChanged ("site" </> filepath) (outputFolder </> filepath)

run :: Command -> Options -> IO ()
run (Build _) Options{..} = shake options do
  pure ()
  where
    options = shakeOptions
      { shakeVerbosity = verbosity
      , shakeThreads   = jobs
      , shakeColor     = True
      , shakeFiles     = "_build"
      }
run (Watch {}) _ = error "TODO: watch"

parseOptions :: A.Parser Options
parseOptions = do
  command <- A.hsubparser (mconcat
    [ A.command "build" (A.info pBuild (A.progDesc "Build the site"))
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
  run options.command options

{-

import           Blog.MMark
import           Blog.Ninja
import           Data.Aeson            (Value (..), (.=))
import           Data.Foldable
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Set
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Lazy        as TextL
import qualified Data.Text.Lazy.IO     as TextL
import           Data.Time.Clock
import           Options.Applicative
import qualified System.FilePath.Glob  as Glob
import qualified Text.Megaparsec.Error as Mega
import qualified Text.Mustache         as Stache

newtype Tag = Tag Text
  deriving (Eq, Ord)

data Post = Info
  { title     :: Text,
    slug      :: Text,
    published :: UTCTime,
    updated   :: Maybe UTCTime,
    tags      :: [Tag],
    other     :: Map Text Text
  }

data Metadata = Meta
  { posts  :: [Post]
  , drafts :: [Post]
  , tags   :: Set Tag
  }

data Options = Options
  { command :: Command
  }

data RenderOptions = RenderOptions
  { template   :: Maybe Text
  , preprocess :: Bool
  , file       :: Text
  }

data MetadataOptions = MetadataOptions
  { clean :: Bool
  , files :: [Text]
  }

data Command
  = Generate
  | Render RenderOptions
  | Metadata MetadataOptions

options :: Parser Options
options = do
  command <- hsubparser (mconcat
    [ command "generate" (info pGenerate (progDesc "generate build.ninja"))
    , command "metadata" (info pMetadata (progDesc "generate post metadata"))
    , command "render" (info pRender (progDesc "render a page"))
    ]) <|> pure Generate

  pure Options{..}

  where
    pGenerate = pure Generate
    pMetadata = fmap Metadata $ MetadataOptions
      <$> switch (long "clean" <> short 'c')
      <*> some (strArgument $ metavar "FILES" <> action "file")
    pRender = fmap Render $ RenderOptions
      <$> optional (strOption $ long "template" <> short 't' <> metavar "TEMPLATE" <> action "file")
      <*> switch (long "preprocess" <> short 'p')
      <*> (strArgument $ metavar "FILE" <> action "file")

config :: Map Text SomePretty
config =
  [ ("site", "_site")
  , ("builddir", ".cache")
  ]

run :: Command -> Options -> IO ()
run Generate Options{} = do
  hsFiles <- (driver :) . (fmap Text.pack) <$> do
     Glob.globDir1 (Glob.compile "*.hs") "Blog"

  writeNinja do
    traverse_ (uncurry variable) $ Map.toList config
    generator driver hsFiles ["generate"]
  where
    driver = "Blog.hs"
run (Metadata MetadataOptions{..}) Options{} = do
  print (clean, files)
run (Render RenderOptions{..}) Options{} = do
  source <- do
    src <- Text.readFile (Text.unpack file)
    if not preprocess then pure src
    else case Stache.compileMustacheText (Stache.PName file) src of
      Left errs -> fail $ Mega.errorBundlePretty errs
      Right t -> pure . TextL.toStrict $ Stache.renderMustache t (Object baseMeta)
  doc@Doc{..} <- parse file source
  case template of
    Just template -> do
      t <- Stache.compileMustacheDir (Stache.PName template) "templates"
      let sbody = TextL.toStrict doc.body
      TextL.putStrLn $ Stache.renderMustache t (Object $ baseMeta <> meta <> ("body" .= sbody))
    _ ->
       TextL.putStrLn doc.body
  where
    author, email, github :: String
    author = "Emeka Nkurumeh"
    email = "e.nk@caltech.edu"
    github = "https://github.com/emekoi"

    siteLang, siteTitle, siteSource, siteURL :: String
    siteLang = "en"
    siteTitle = author ++ "'s Blog"
    siteSource = "https://github.com/emekoi/emekoi.github.io"
    siteURL = "https://emekoi.github.io"

    baseMeta =
      [ "author" .= author
      , "lang" .= siteLang
      , "github" .= github
      , "site-source" .= siteSource
      , "site-title" .= siteTitle
      , "site-url" .= siteURL
      , "email" .= email
      ]

main :: IO ()
main = do
  options <- execParser (info options idm)
  run options.command options

-}
