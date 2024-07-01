module Blog where

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
import qualified System.FilePath      as FP
import qualified System.FilePath.Glob as Glob

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
  -- | Pandoc
  | Metadata Bool [Text]

options :: Parser Options
options = do
  command <- hsubparser (mconcat
    [ command "generate" (info pGenerate (progDesc "generate build.ninja"))
    , command "metadata" (info pMetadata (progDesc "generate post metadata"))
    ]) <|> pure Generate

  pure Options{..}

  where
    pGenerate = pure Generate
    pMetadata = Metadata
      <$> switch (long "clean" <> short 'c')
      <*> some (strArgument $ metavar "FILES")

config :: Map Text SomePretty
config =
  [ ("site", "_site")
  , ("builddir", ".cache")
  ]

run :: Command -> Options -> IO ()
run Generate Options{} = do
  hsFiles <- fmap Text.pack <$> do
    let pat = Glob.compile "*.hs"
    files1 <- fmap FP.takeFileName <$> Glob.globDir1 pat "."
    files2 <- Glob.globDir1 pat "Blog"
    pure $ files1 ++ files2

  writeNinja do
    traverse_ (uncurry variable) $ Map.toList config
    generator "Blog.hs" hsFiles ["generate"]
run (Metadata clean files) Options{} = do
  print (clean, files)

main :: IO ()
main = do
  options <- execParser (info options idm)
  run options.command options
