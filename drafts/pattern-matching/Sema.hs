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
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Graph                 qualified as Graph
import Data.IntMap.Strict         (IntMap)
import Data.IntMap.Strict         qualified as IntMap
import Data.IntSet                (IntSet)
import Data.IntSet                qualified as IntSet
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Sequence              (Seq (..))
import Data.IORef                 (IORef)
import Data.IORef                 qualified as IORef
import Data.Text                  (Text)
import Parser
-- import Prettyprinter              ((<+>))
import Error
import Prettyprinter              qualified as P
import Witherable
import Data.Default.Class

newIORef :: (MonadIO m) => x -> m (IORef x)
newIORef = liftIO . IORef.newIORef

readIORef :: (MonadIO m) => IORef x -> m x
readIORef = liftIO . IORef.readIORef

writeIORef :: (MonadIO m) => IORef x -> x -> m ()
writeIORef r = liftIO . IORef.writeIORef r

modifyIORef :: (MonadIO m) => IORef x -> (x -> x) -> m ()
modifyIORef k = liftIO . IORef.modifyIORef k

modifyIORef' :: (MonadIO m) => IORef x -> (x -> x) -> m ()
modifyIORef' k = liftIO . IORef.modifyIORef' k

data UsageInfo r = Usage
  { usageInfo :: IntMap (r, IntSet)
  , termIds   :: Map Text Int
  , fresh     :: Int
  }
  deriving (Show)

instance Default (UsageInfo r) where
  def = Usage mempty mempty 0

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
usageDepend x r d = modify' \Usage{..} ->
  Usage { usageInfo = IntMap.insert x (r, d) usageInfo, .. }

usageDepend' :: (Semigroup r, MonadState (UsageInfo r) m) => Int -> r -> IntSet -> m ()
usageDepend' x r d = modify' \Usage{..} ->
  Usage { usageInfo = IntMap.insertWith (<>) x (r, d) usageInfo, .. }

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

data SemaState = SemaState
  { fileName :: FilePath
  , fresh    :: IORef Int
  , dataCons :: IORef (Map Text Int)
  , dataSpans :: IORef (IntMap (Range, Int))
  }

newtype Sema s
  = Sema { runSema :: ReaderT SemaState IO s }
  deriving (Applicative, Functor, Monad, MonadReader SemaState, MonadIO)

runSema :: FilePath -> Sema s -> IO s
runSema fp (Sema s) = do
  ctx <- SemaState fp
    <$> newIORef 0
    <*> newIORef mempty
    <*> newIORef mempty
  runReaderT s ctx

fresh :: Sema Int
fresh = ask >>= \s -> do
  fresh <- readIORef s.fresh
  modifyIORef' s.fresh succ
  pure fresh

getDataCon :: Name a -> Sema Int
getDataCon (Name _ n) = do
  s <- ask
  Map.lookup n <$> readIORef s.dataCons >>= \case
    Just i -> pure i
    Nothing -> do
      i <- fresh
      modifyIORef' s.dataCons (Map.insert n i)
      pure i

collectDataCons :: Module -> Sema (Seq (ExprDecl Range))
collectDataCons (Module ds) = (`witherM` ds) \case
  DData _ c xs -> do
    i <- getDataCon c
    mapM_ f xs
    s <- ask
    (IntMap.lookup i <$> readIORef s.dataSpans) >>= \case
      Just (r, _) -> throwError'
        [ "redefinition of"
        , P.squotes $ P.pretty c
        ]
        [ (r2p s.fileName r, This "first defined here")
        , (r2p s.fileName c, This "redefined here")
        ]
      Nothing ->
        modifyIORef' s.dataSpans (IntMap.insert i (range c, length xs))
    pure Nothing
  DExpr e@(ExprDecl {}) -> pure (Just e)
  where
    f (DataCon _ c xs) = getDataCon c *> mapM_ f xs

-- data SemaState = SemaState
--   { fresh     :: Int
--   , dataCons  :: Map Text Int
--   , dataSpans :: IntMap (Range, Int)
--   }

-- newtype Sema s
--   = Sema { runSema :: State SemaState s }
--   deriving (Applicative, Functor, Monad, MonadState SemaState)

-- runSema :: Sema s -> (s, SemaState)
-- runSema (Sema s) = runState s (SemaState 0 mempty mempty)

-- getDataCon :: Name a -> Sema Int
-- getDataCon (Name _ n) = do
--   SemaState{..} <- get
--   case Map.lookup n dataCons of
--     Just i -> pure i
--     Nothing -> do
--       put SemaState
--         { fresh = fresh + 1
--         , dataCons = Map.insert n fresh dataCons
--         , ..
--         }
--       pure fresh

-- collectDataCons :: Module -> Sema (Seq (Decl Range))
-- collectDataCons (Module fp ds) = (`filterM` ds) \case
--   DData _ c xs -> do
--     i <- getDataCon c
--     mapM_ f xs
--     SemaState{..} <- get
--     case IntMap.lookup i dataSpans of
--       Just (r, _) -> throwError'
--         [ "redefinition of"
--         , P.squotes $ P.pretty c
--         ]
--         [ (r2p fp r, This "first defined here")
--         , (r2p fp c, This "redefined here")
--         ]
--       Nothing ->
--         put SemaState
--           { dataSpans = IntMap.insert i (range c, length xs) dataSpans, .. }
--     pure False
--   DExpr {} -> pure True
--   where
--     f (DataCon _ c xs) = getDataCon c *> mapM_ f xs
--     filterM p = foldr `flip` pure Empty $ \x ->
--       liftA2 (\b -> if b then (x :<|) else id) (p x)

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
