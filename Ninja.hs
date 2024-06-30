{-# OPTIONS_GHC -Wall -Wextra -Wno-name-shadowing #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Ninja where

import           Control.Monad.Reader      (ReaderT (..), ask, lift)
import           Data.Default
import           Data.IORef                (IORef, modifyIORef', newIORef,
                                            readIORef)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Prettyprinter             (Pretty (..), (<+>))
import qualified Prettyprinter.Internal    as P
import qualified Prettyprinter.Render.Text as P
import           System.Environment        (getExecutablePath)
import qualified System.IO                 as SIO
import Data.String (IsString(..))

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

type NinjaM r = ReaderT (IORef Ninja) IO r

runNinja :: NinjaM () -> IO Ninja
runNinja m = do
  r <- newIORef (Ninja mempty mempty)
  () <- runReaderT m r
  readIORef r

-- rule :: Rule -> NinjaM ()
-- rule t = do
--   ninja <- ask
--   lift $ modifyIORef' ninja \Ninja{..} ->
--     Ninja { rules = Map.insert t.rule.name t.rule rules, targets = t : targets }

target :: Target -> NinjaM ()
target t = do
  ninja <- ask
  lift $ modifyIORef' ninja \Ninja{..} ->
    Ninja { rules = Map.insert t.rule.name t.rule rules, targets = t : targets }

generator :: Text -> NinjaM ()
generator main = do
  self <- Text.pack <$> lift getExecutablePath

  target def
    { rule = Rule "generate" self [("generator", Pretty True)]
    , input = Input [] [self] []
    , output = Output ["build.ninja"] []
    , variables = []
    }

  -- target def
  --   { rule = Rule "ghc-depends" "ghc -M $in -dep-makefile $out"
  --     [ ("depfile", "$out.d")
  --     , ("deps", "gcc")
  --     ]
  --   , output = Output ["Ninja.d"] []
  --   , input = Input ["Ninja.hs"] [] []
  --   , variables = []
  --   }

  target def
    { rule = Rule "ghc" "ghc --make $in -o $out -outputdir .cache" []
    , output = Output [self] [".cache/Main.hi", ".cache/Main.o"]
    , input = Input [main] [] []
    -- , variables = [("restat", "True")]
    }

  -- target def
  --   { rule = ghc
  --   , output = Output ["Ninja.o"] ["Ninja.hi"]
  --   , input = Input ["Ninja.hs"] [] []
  --   }

  -- target def
  --   { rule = ghc
  --   , output = Output ["Ninja.o"] ["Ninja.hi"]
  --   , input = Input ["Ninja.hs"] [] []
  --   }

  -- where
  --   ghc = Rule "ghc" "ghc -M $in -dep-makefile $$(basename $in .hs).d && ghc $in -o $out"
  --     [ ("depfile", "$out.d")
  --     , ("deps", "gcc")
  --     ]

writeNinja :: NinjaM () -> IO ()
writeNinja m =
  SIO.withFile "build.ninja" SIO.WriteMode \handle -> do
    ninjaDoc <- pretty <$> runNinja m
    P.hPutDoc handle (ninjaDoc <> P.hardline <> "default build.ninja")
    SIO.hPutChar handle '\n'
