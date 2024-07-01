module Blog.Ninja
  ( Input (..)
  , Output (..)
  , Rule (..)
  , Target(..)
  , SomePretty (..)
  , NinjaM
  , default_
  , generator
  , rule
  , target
  , variable
  , writeNinja
  ) where

import           Control.Monad.Reader      (ReaderT (..), ask, lift)
import           Data.Default
import qualified Data.IORef                as IR
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.String               (IsString (..))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Prettyprinter             (Pretty (..), (<+>))
import qualified Prettyprinter.Internal    as P
import qualified Prettyprinter.Render.Text as P
import           System.Environment        (getExecutablePath)
import qualified System.IO                 as SIO

data SomePretty where
  Pretty :: Pretty p => p -> SomePretty

instance Pretty SomePretty where
  pretty (Pretty p) = pretty p

instance IsString SomePretty where
  fromString s = Pretty s

pVar :: (Pretty p) => Text -> p -> P.Doc a
pVar x v = pretty x <+> "=" <+> pretty v

(</>) :: P.Doc a -> P.Doc a -> P.Doc a
P.Empty </> y = y
x </> P.Empty = x
x </> y       = x <+> y

data Rule = Rule
  { name      :: Text
  , command   :: Text
  , variables :: Map Text SomePretty
  }

instance Default Rule where
  def = Rule mempty mempty mempty

instance Pretty Rule where
  pretty Rule{..} = P.nest 2 . P.vsep $
    "rule" <+> P.pretty name
    : pVar "command" command
    : (uncurry pVar <$> Map.toList variables)

data Input = Input
  { explicit :: [Text]
  , implicit :: [Text]
  , ordered  :: [Text]
  }

instance Default Input where
  def = Input mempty mempty mempty

instance Pretty Input where
  pretty Input{..} =
    hsep explicit
    </> (if null implicit then mempty else "|" <+> hsep implicit)
    </> (if null ordered then mempty else "||" <+> hsep ordered)
    where hsep = P.hsep . fmap pretty

data Output = Output
  { explicit :: [Text]
  , implicit :: [Text]
  }

instance Default Output where
  def = Output mempty mempty

instance Pretty Output where
  pretty Output{..} =
    hsep explicit
    </> (if null implicit then mempty else "|" <+> hsep implicit)
    where hsep = P.hsep . fmap pretty

data Target = Target
  { rule      :: Rule
  , input     :: Input
  , output    :: Output
  , variables :: Map Text SomePretty
  }

instance Default Target where
  def = Target def def def def

instance Pretty Target where
  pretty Target{..} =
    P.nest 2 . P.vsep  $ "build" <+> pretty output <> ":" <+> pretty rule.name <+> pretty input
    : (uncurry pVar <$> Map.toList variables)

data Ninja = Ninja
  { rules     :: Map Text Rule
  , targets   :: [Target]
  , variables :: Map Text SomePretty
  , defaults  :: [Text]
  }

instance Pretty Ninja where
  pretty Ninja{..} = P.vsep $
    (uncurry pVar <$> Map.toList variables)
    ++ fmap pretty (Map.elems rules)
    ++ fmap pretty targets
    ++ [P.hsep $ "default" : fmap pretty defaults]

type NinjaM r = ReaderT (IR.IORef Ninja) IO r

rule :: Rule -> NinjaM ()
rule r = do
  ninja <- ask
  lift $ IR.modifyIORef' ninja \Ninja{..} ->
    Ninja { rules = Map.insert r.name r rules, .. }

target :: Target -> NinjaM ()
target t = do
  ninja <- ask
  lift $ IR.modifyIORef' ninja \Ninja{..} ->
    Ninja
      { rules = Map.insert t.rule.name t.rule rules
      , targets = t : targets
      , ..
      }

variable :: Text -> SomePretty -> NinjaM ()
variable x v = do
  ninja <- ask
  lift $ IR.modifyIORef' ninja \Ninja{..} ->
    Ninja { variables = Map.insert x v variables, .. }

default_ :: Text -> NinjaM ()
default_ t = do
  ninja <- ask
  lift $ IR.modifyIORef' ninja \Ninja{..} ->
    Ninja { defaults = t : defaults, .. }

-- generator :: Text -> [Text] -> [Text] -> NinjaM ()
-- generator main source args = do
--   self <- Text.pack <$> lift getExecutablePath

--   target def
--     { rule = Rule "generate" (Text.unwords $ self : args) [("generator", Pretty True)]
--     , input = Input [] [self] []
--     , output = Output ["build.ninja"] []
--     }

--   default_ "build.ninja"

--   target def
--     { rule = Rule
--         "ghc"
--         "ghc --make -outputdir $builddir -o $out -main-is $$(basename $in .hs) $in"
--         []
--     , output = Output [self] (hsOut source)
--     , input = Input [main] source []
--     , variables = [("restat", "true")]
--     }

--   where
--     hsOut files = foldMap (\x ->
--       let a = (Text.unpack x) in [
--         Text.pack $ "$builddir" FP.</> (a FP.-<.> ext) | ext <- ["hi", "o"]
--       ]) files

generator :: Text -> [Text] -> [Text] -> NinjaM ()
generator main source args = do
  self <- Text.pack <$> lift getExecutablePath

  target def
    { rule = Rule "generate" (Text.unwords $ self : args) [("generator", Pretty True)]
    , input = Input [] [self] []
    , output = Output ["build.ninja"] []
    }

  default_ "build.ninja"

  target def
    { rule = Rule
        "cabal"
        "cabal -j build"
        []
    , output = Output [self] []
    , input = Input [main] source []
    }

writeNinja :: NinjaM () -> IO ()
writeNinja m = do
  r <- IR.newIORef (Ninja mempty mempty mempty mempty)
  SIO.withFile "build.ninja" SIO.WriteMode \handle -> do
    ninjaDoc <- pretty <$> (runReaderT m r >> IR.readIORef r)
    P.hPutDoc handle ninjaDoc
    SIO.hPutChar handle '\n'
