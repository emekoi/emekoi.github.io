{-# OPTIONS_GHC -Wall -Wextra -Wno-name-shadowing #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE NoFieldSelectors      #-}

-- import Control.Monad
-- import Data.Time.Format
import System.Environment

import Data.Map.Strict (Map)
import Data.Text (Text)
import Prettyprinter (Pretty(..),(<+>))

import Data.IORef
import Control.Monad.Reader

-- import System.FilePath.Glob (Pattern)

import Data.Text qualified as Text
import Data.Map.Strict qualified as Map
import System.FilePath.Glob qualified as Glob
import Prettyprinter qualified as P
import Prettyprinter.Internal qualified as P
import Prettyprinter.Render.Text qualified as P
import System.IO

pVar :: (Pretty p) => Text -> p -> P.Doc a
pVar x v = pretty x <+> "=" <+> pretty v

(</>) :: P.Doc a -> P.Doc a -> P.Doc a
P.Empty </> y = y
x </> P.Empty = x
x </> y = x <+> y

data Rule = Rule
  { name      :: Text
  , command   :: Text
  , variables :: Map Text Text
  }

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

instance Pretty Output where
  pretty Output{..} =
    hsep explicit
    </> (if null implicit then mempty else "|" <+> hsep implicit)
    where hsep = P.hsep . fmap pretty

data Target = Target
  { rule      :: Rule
  , input     :: Input
  , output    :: Output
  , variables :: Map Text Text
  }

instance Pretty Target where
  pretty Target{..} =
    P.nest 2 . P.vsep  $ "build" <+> pretty output <> ":" <+> pretty rule.name <+> pretty input
    : (uncurry pVar <$> Map.toList variables)

copyRule :: Rule
copyRule = Rule
  { name      = "copy"
  , command   = "cp $in $out"
  , variables = []
  }

-- copy ::  -> Text -> Target
-- copy src dst = Target copyRule (Input [] [] []) (Output [] [])

-- css :: IO Target
-- css = do
--   files <- Glob.glob "css/*."
--   pure $ Target copy [] []

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
  io <- newIORef (Ninja mempty mempty)
  () <- runReaderT m io
  readIORef io

-- rule :: Rule -> NinjaM ()
-- rule r = do
--   ninja <- ask
--   lift $ modifyIORef' ninja (\Ninja{..} -> Ninja { rules = Map.insert r.name r rules, .. })

target :: Target -> NinjaM ()
target t = do
  ninja <- ask
  lift $ modifyIORef' ninja \Ninja{..} ->
    Ninja { rules = Map.insert t.rule.name t.rule rules, targets = t : targets }

-- ninja :: Input -> NinjaM ()
-- ninja input = do
--   -- self <- Text.pack <$> lift getProgName
--   self <- Text.pack <$> lift getExecutablePath
--   target Target { rule = rule self , variables = [] , .. }
--   where
--     rule prog = Rule "ninja" prog [("generator", "true")]
--     output = Output { explicit = ["build.ninja"] , implicit = [] }

ninja :: NinjaM ()
ninja = do
  -- self <- Text.pack <$> lift getProgName
  self <- Text.pack <$> lift getExecutablePath

  target Target
    { rule = Rule
      { name      = "ghc"
      , command   = "ghc --make $in -o $out"
      , variables = []
      }
    , output = Output ["Ninja"] ["Ninja.hi", "Ninja.o"]
    , input = Input ["Ninja.hs"] [] []
    , variables = []
    }

  target Target
    { rule = ninja self
    , input = Input [] ["Ninja"] []
    , output = Output ["build.ninja"] []
    , variables = []
    }
  where
    ninja prog = Rule "ninja" prog [("generator", "true")]

main :: IO ()
main = do
  files <- fmap Text.pack <$> Glob.glob "*.hs"
  ninjaFile <- runNinja ninja

  withFile "build.ninja" WriteMode \handle -> do
    P.hPutDoc handle (pretty ninjaFile <> P.hardline <> "default build.ninja")
    hPutChar handle '\n'
