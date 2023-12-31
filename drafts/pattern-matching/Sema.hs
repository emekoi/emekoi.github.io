{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module Sema
    ( module Sema
    ) where

-- import Control.Monad
-- import Control.Monad.Except
-- import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Graph                 qualified as Graph
import Data.IntMap.Strict         (IntMap)
import Data.IntMap.Strict         qualified as IntMap
import Data.IntSet                (IntSet)
import Data.IntSet                qualified as IntSet
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Sequence              (Seq (..))
-- import Data.IORef                 (IORef)
-- import Data.IORef                 qualified as IORef
import Control.Exception
import Data.Text                  (Text)
import Data.Text                  qualified as Text
import Lexer                      (AlexPosn (..))
import Parser
-- import Prettyprinter              ((<+>))
import Error.Diagnose
import Prettyprinter              qualified as P
import Prettyprinter.Render.Text  qualified as P

data ErrMsg where ErrMsg :: P.Doc a -> ErrMsg

instance P.Pretty ErrMsg where
  pretty (ErrMsg msg) = P.pretty
    . Text.unpack
    . P.renderStrict
    . P.layoutPretty P.defaultLayoutOptions $ msg

newtype SemaError
  = SemaError (Report ErrMsg)

instance Show SemaError where
  show = error "uncaught SemaError"

instance Exception SemaError

semaError :: [P.Doc a] -> [(FilePath, Range, Marker (P.Doc a))] -> error
semaError msg xs = throw . SemaError $ err Nothing (ErrMsg (P.hsep msg)) (f <$> xs) []
  where
    f (fp, Range (AlexPn _ l1 c1) (AlexPn _ l2 c2), d) = (Position (l1, c1) (l2, c2) fp, g ErrMsg d)
    g f (This d)  = This (f d)
    g f (Where d) = Where (f d)
    g f (Maybe d) = Maybe (f d)

data UsageInfo r = Usage
  { usageInfo :: IntMap (r, IntSet)
  , termIds   :: Map Text Int
  , fresh     :: Int
  }
  deriving (Show)

usageId :: (MonadState (UsageInfo r) m) => Text -> m Int
usageId x = state \u@(Usage {..}) ->
  case Map.lookup x termIds of
    Just i -> (i, u)
    Nothing ->
      ( fresh
      , u
        { fresh = fresh + 1
        , termIds = Map.insert x fresh termIds}
      )

usageDepend :: (MonadState (UsageInfo r) m) => Int -> r -> IntSet -> m ()
usageDepend x r d = modify' \u -> u { usageInfo = IntMap.insert x (r, d) u.usageInfo }

usageDepend' :: (Semigroup r, MonadState (UsageInfo r) m) => Int -> r -> IntSet -> m ()
usageDepend' x r d = modify' \u -> u { usageInfo = IntMap.insertWith (<>) x (r, d) u.usageInfo }

usageSCCs :: (MonadState (UsageInfo r) m) => m [Graph.SCC r]
usageSCCs = gets \(Usage {..}) ->
  Graph.stronglyConnComp $ IntMap.foldrWithKey go [] usageInfo
  where go x (k, xs) r = (k, x, IntSet.toList xs) : r

data Type
  = TOpaque
  | TUnknown
  | TInt
  | TString
  | TData Text
  deriving (Eq)

-- data SemaState = SemaState
--   { fresh    :: Int
--   , dataCons :: Map Text Int
--   , dataSpans :: IntMap Int
--   }

-- newtype Sema s
--   = Sema { runSema :: ReaderT SemaState IO s }
--   deriving (Applicative, Functor, Monad, MonadReader SemaState)

data SemaState = SemaState
  { fresh     :: Int
  , dataCons  :: Map Text Int
  , dataSpans :: IntMap Int
  }

newtype Sema s
  = Sema { runSema :: State SemaState s }
  deriving (Applicative, Functor, Monad, MonadState SemaState)

runSema :: Sema s -> (s, SemaState)
runSema (Sema s) = runState s (SemaState 0 mempty mempty)

getDataCon :: Name a -> Sema Int
getDataCon (Name _ n) = do
  SemaState{..} <- get
  case Map.lookup n dataCons of
    Just i -> pure i
    Nothing -> do
      put SemaState
        { fresh = fresh + 1
        , dataCons = Map.insert n fresh dataCons
        , ..
        }
      pure fresh

collectDataCons :: Module -> Sema (Seq (Decl Range))
collectDataCons (Module fp ds) = (`filterM` ds) \case
  DData r c xs -> do
    i <- getDataCon c
    mapM_ f xs
    SemaState{..} <- get
    case IntMap.lookup i dataSpans of
      Just _ -> semaError
        [ "redefinition of"
        , P.squotes $ P.pretty c
        ]
        [ (fp, r, This "redefined here")
        ]
      Nothing ->
        put SemaState
          { dataSpans = IntMap.insert i (length xs) dataSpans, .. }
    pure False
  DExpr {} -> pure True
  where
    f (DataCon _ c xs) = getDataCon c *> mapM_ f xs
    filterM p = foldr `flip` pure Empty $ \x ->
      liftA2 (\b -> if b then (x :<|) else id) (p x)

-- tyCheck :: Seq (Decl Range) -> Seq (Decl Type)
-- tyCheck xs = error "TODO"

-- data Name a
--   = Name a Text

-- data Pattern a
--   = PWild a
--   | PInt a Integer
--   | PString a Text
--   | PAs a (Name a) (Pattern a)
--   | PData a (Name a) (Seq (Pattern a))
--   | POr a (Seq (Pattern a))

-- data DataCon a
--  = DataCon a (Name a) (Seq (DataCon a))

-- data Decl a
--   = DExpr a (Name a) (Seq (Pattern a)) (Expr a)
--   | DData a (Name a) (Seq (DataCon a))

-- data Alt a
