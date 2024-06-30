{-# OPTIONS_GHC -Wall -Wextra -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Ninja
  ( Input (..)
  , Output (..)
  , Rule (..)
  , Target(..)
  , SomePretty (..)
  , target
  , generator
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
import qualified System.FilePath           as FP
import qualified System.FilePath.Glob      as Glob
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
  { rules   :: Map Text Rule
  , targets :: [Target]
  }

instance Pretty Ninja where
  pretty Ninja{..} = P.vsep $
    fmap pretty (Map.elems rules) ++ fmap pretty targets

type NinjaM r = ReaderT (IR.IORef Ninja) IO r

target :: Target -> NinjaM ()
target t = do
  ninja <- ask
  lift $ IR.modifyIORef' ninja \Ninja{..} ->
    Ninja { rules = Map.insert t.rule.name t.rule rules, targets = t : targets }

generator :: Text -> NinjaM ()
generator main = do
  self <- Text.pack <$> lift getExecutablePath

  hsFiles <- lift $ Glob.glob "*.hs"

  target def
    { rule = Rule "generate" self [("generator", Pretty True)]
    , input = Input [] [self] []
    , output = Output ["build.ninja"] []
    }

  target def
    { rule = Rule
        "ghc"
        "ghc --make -outputdir .cache -o $out -main-is $$(basename $in .hs) $in"
        []
    , output = Output [self] (hsOut hsFiles ++ [".cache"])
    , input = Input [main] (Text.pack <$> hsFiles) []
    , variables = [("restat", "true")]
    }

  where
    hsOut files = foldMap (\x ->
      let (a, b) = FP.splitFileName x in [
        Text.pack $ a FP.</> ".cache" FP.</> (b FP.-<.> ext) | ext <- ["hi", "o"]
      ]) files

writeNinja :: NinjaM () -> IO ()
writeNinja m = do
  r <- IR.newIORef (Ninja mempty mempty)
  SIO.withFile "build.ninja" SIO.WriteMode \handle -> do
    ninjaDoc <- pretty <$> (runReaderT m r >> IR.readIORef r)
    P.hPutDoc handle (ninjaDoc <> P.hardline <> "default build.ninja")
    SIO.hPutChar handle '\n'
