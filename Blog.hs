module Blog where

import           Blog.MMark
import           Blog.Ninja
import           Data.Default
import           Data.Foldable
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Set
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time.Clock
import           Options.Applicative
import qualified System.FilePath.Glob as Glob
import qualified Data.Text.IO          as Text

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

copy :: Rule
copy = def
  { name = "copy"
  , command = "cp $in $out"
  }

data Metadata = Meta
  { posts  :: [Post]
  , drafts :: [Post]
  , tags   :: Set Tag
  }

-- newtype Task c k v = Task
--   { runTask :: forall f. c f => (k -> f v) -> f v
--   }

data Options = Options
  { command :: Command
  }

data Command
  = Generate
  | Render Text
  | Metadata Bool [Text]

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
    pMetadata = Metadata
      <$> switch (long "clean" <> short 'c')
      <*> some (strArgument $ metavar "FILES" <> action "file")
    pRender = Render
      <$> (strArgument $ metavar "FILE" <> action "file")

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
run (Metadata clean files) Options{} = do
  print (clean, files)
run (Render file) Options{} = do
  source <- Text.readFile (Text.unpack file)
  case parse file source of
    Left err -> putStrLn err
    Right Doc{..} ->
      print body

main :: IO ()
main = do
  options <- execParser (info options idm)
  run options.command options
