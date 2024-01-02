{-# OPTIONS_GHC -Wno-unused-top-binds   #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedLists            #-}

module Sema
    ( Sema
    , runSema
    , Type (..)
    , analyze
    ) where

import Control.Category    ((>>>))
import Control.Exception
import Control.Monad
import Control.Monad.Reader
-- import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Default.Class
import Data.Functor
-- import Data.Graph                 qualified as Graph
import Data.IntMap.Strict   (IntMap)
import Data.IntMap.Strict   qualified as IntMap
import Data.IntSet          (IntSet)
-- import Data.IntSet                qualified as IntSet
import Data.IORef           (IORef)
import Data.IORef           qualified as IORef
import Data.Map.Strict      (Map)
import Data.Map.Strict      qualified as Map
import Data.Sequence        (Seq (..))
import Data.Sequence        qualified as Seq
import Data.Text            (Text)
import Error
import Parser               hiding (Type)
import Prettyprinter        qualified as P
import Witherable
import Lens.Micro
import GHC.Stack

newIORef :: (MonadIO m) => x -> m (IORef x)
newIORef = liftIO . IORef.newIORef

readIORef :: (MonadIO m) => IORef x -> m x
readIORef = liftIO . IORef.readIORef

-- writeIORef :: (MonadIO m) => IORef x -> x -> m ()
-- writeIORef r = liftIO . IORef.writeIORef r

-- modifyIORef' :: (MonadIO m) => IORef x -> (x -> x) -> m ()
-- modifyIORef' k = liftIO . IORef.modifyIORef k

modifyIORef :: (MonadIO m) => IORef x -> (x -> x) -> m ()
modifyIORef k = liftIO . IORef.modifyIORef' k

data UsageInfo r = Usage
  { usageInfo :: IntMap (r, IntSet)
  , termIds   :: Map Text Int
  , fresh     :: Int
  }
  deriving (Show)

instance Default (UsageInfo r) where
  def = Usage mempty mempty 0

-- usageId :: (MonadState (UsageInfo r) m) => Text -> m Int
-- usageId x = state \u@(Usage {..}) ->
--   case Map.lookup x termIds of
--     Just i -> (i, u)
--     Nothing ->
--       ( fresh
--       , u
--         { fresh = fresh + 1
--         , termIds = Map.insert x fresh termIds}
--       )

-- usageDepend :: (MonadState (UsageInfo r) m) => Int -> r -> IntSet -> m ()
-- usageDepend x r d = modify' \Usage{..} ->
--   Usage { usageInfo = IntMap.insert x (r, d) usageInfo, .. }

-- usageDepend' :: (Semigroup r, MonadState (UsageInfo r) m) => Int -> r -> IntSet -> m ()
-- usageDepend' x r d = modify' \Usage{..} ->
--   Usage { usageInfo = IntMap.insertWith (<>) x (r, d) usageInfo, .. }

-- usageSCCs :: (MonadState (UsageInfo r) m) => m [Graph.SCC r]
-- usageSCCs = gets \(Usage {..}) ->
--   Graph.stronglyConnComp $ IntMap.foldrWithKey go [] usageInfo
--   where go x (k, xs) r = (k, x, IntSet.toList xs) : r

newtype TypeId
  = TypeId Int
  deriving (Eq, Num, Ord, Show)

newtype ConId
  = ConId Int
  deriving (Eq, Num, Ord, Show)

data Type
  = TUnknown
  | TData Text TypeId
  deriving (Eq, Show)

pattern TInt :: Type
pattern TInt = TData "Int" (-1)

pattern TString :: Type
pattern TString = TData "String" (-2)

instance P.Pretty Type where
  pretty TUnknown    = "?"
  pretty (TData c _) = P.pretty c

data TypeInfo = TypeInfo
  { name :: Name Range
  , id   :: TypeId
  , span :: Int
  }
  deriving (Show)

tyInfo2Ty :: TypeInfo -> Type
tyInfo2Ty t = TData (getName t.name) t.id

data ConInfo = ConInfo
  { name   :: Name Range
  , id     :: ConId
  , fields :: Seq TypeId
  , typeOf :: TypeId
  }
  deriving (Show)

data SemaState = SemaState
  { fileName    :: FilePath
  , fresh       :: IORef Int
  , types       :: IORef (Map Text TypeId)
  , typeInfo    :: IORef (IntMap TypeInfo)
  , dataCons    :: IORef (Map Text ConId)
  , dataConInfo :: IORef (IntMap ConInfo)
  }

newtype Sema s
  = Sema (ReaderT SemaState IO s)
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader SemaState)

dumpSema :: Sema ()
dumpSema = do
  SemaState{..} <- ask
  readIORef types >>= liftIO . print
  readIORef typeInfo >>= liftIO . print
  readIORef dataCons >>= liftIO . print
  readIORef dataConInfo >>= liftIO . print

throwSema :: ErrMsg -> [(Range, Marker ErrMsg)] -> Sema a
throwSema msg xs = Sema $ ReaderT \s -> throwIO . Error $
  [Err' msg (first (r2p s.fileName) <$> xs)]

errQuote :: (P.Pretty p) => p -> ErrMsg
errQuote = ErrPretty P.squotes

runSema :: FilePath -> Sema s -> IO s
runSema fp (Sema s) = do
  ctx <- SemaState fp
    <$> newIORef 0
    <*> newIORef initTypes
    <*> newIORef initTypeInfo
    <*> newIORef mempty
    <*> newIORef mempty
  runReaderT s ctx
  where
    initTypes = Map.fromList [("Int", -1), ("String", -2)]
    initTypeInfo = IntMap.fromList []

fresh :: Sema Int
fresh = ask >>= \s -> do
  fresh <- readIORef s.fresh
  modifyIORef s.fresh succ
  pure fresh

-- fixity helper
(.-) :: (a -> b) -> (b -> c) -> (a -> c)
(.-) = (>>>)

findTypeId :: Name Range -> Sema TypeId
findTypeId (Name r n) = do
  SemaState{types} <- ask
  readIORef types >>= Map.lookup n .- \case
    Just id -> pure id
    Nothing -> throwSema
      [ "unkown type", errQuote n ]
      [ (r, This "used here") ]

findTypeInfo :: HasCallStack => TypeId -> Sema TypeInfo
findTypeInfo (TypeId id) = do
  SemaState{typeInfo} <- ask
  readIORef typeInfo >>= IntMap.lookup id .- \case
    Just id -> pure id
    Nothing -> error
      "compiler error: assigned type id to non-existent type"

makeTypeId :: Name Range -> Sema TypeId
makeTypeId (Name _ n) = do
  SemaState{types} <- ask
  readIORef types >>= Map.lookup n .- \case
    Just id -> pure id
    Nothing -> do
      id <- TypeId <$> fresh
      modifyIORef types (Map.insert n id)
      pure id

findConId :: Name Range -> Sema ConId
findConId (Name r n) = do
  SemaState{dataCons} <- ask
  readIORef dataCons >>= Map.lookup n .- \case
    Just id -> pure id
    Nothing -> throwSema
      [ "unknown constructor", errQuote n ]
      [ (r, This "used here") ]

findConInfo :: HasCallStack => ConId -> Sema ConInfo
findConInfo (ConId id) = do
  SemaState{dataConInfo} <- ask
  readIORef dataConInfo >>= IntMap.lookup id .- \case
    Just i -> pure i
    Nothing -> error
      "compier error: assigned constructor id to non-existent constructor"

makeConId :: Name Range -> Sema ConId
makeConId (Name r c) = do
  SemaState{dataCons} <- ask
  readIORef dataCons >>= Map.lookup c .- \case
    Nothing -> do
      id <- ConId <$> fresh
      modifyIORef dataCons (Map.insert c id)
      pure id
    Just id -> do
      info <- findConInfo id
      throwSema
        [ "redefinition of constructor", errQuote c ]
        [ (range info.name, This "first defined here"), (r, This "redefined here") ]

tyErrorExpect
  :: (HasRange r, P.Pretty a2, P.Pretty a3)
  => ErrMsg -> a2 -> a3 -> r -> Sema a1
tyErrorExpect thing expect got r =
  throwSema [ "expected", thing, "of type", errQuote expect ]
    [ (range r, This [
        thing, "has type", errQuote got
      ])
    ]

-- data TypeInfo = TypeInfo
--   { name :: Name Range
--   , id   :: TypeId
--   , span :: Int
--   }
--   deriving (Show)

-- data ConInfo = ConInfo
--   { name   :: Name Range
--   , id     :: ConId
--   , span   :: Int
--   , typeOf :: TypeId
--   }
--   deriving (Show)

tyCheckDataDecl :: HasCallStack => Name Range -> Seq (DataCon Range) -> Sema ()
tyCheckDataDecl t cs = do
  tid@(TypeId id) <- makeTypeId t
  when (tid < 0) $ throwSema
    [ "redefinition of builtin type", errQuote t ]
    [ (range t, This "redefined here") ]
  SemaState{typeInfo,dataConInfo} <- ask
  readIORef typeInfo >>= IntMap.lookup id .- \case
    Just info -> do
      throwSema
        [ "redefinition of type", errQuote t ]
        [ (range info.name, This "first defined here")
        , (range t, This "redefined here")
        ]
    Nothing -> modifyIORef typeInfo . IntMap.insert id $
      TypeInfo { name = t, id = tid, span = length cs }
  forM_ cs \(DataCon _ name xs) -> do
    id@(ConId cid) <- makeConId name
    fields <- mapM makeTypeId xs
    modifyIORef dataConInfo . IntMap.insert cid $
      ConInfo { typeOf = tid, .. }

-- TODO: use "unification" instead of type equality checks
tyInferPat :: HasCallStack => Pattern Range -> Sema (Pattern (Range, Type))
tyInferPat (PType _ p t@(Name _ tn)) = do
  ty <- TData tn <$> findTypeId t
  tyCheckPat ty p
tyInferPat (PAs r n p) = do
  p' <- tyInferPat p
  let (_, t) = info p'
  pure (PAs (r, t) ((,t) <$> n) p')
tyInferPat (PData r c ps) = do
  cinfo <- findConId c >>= findConInfo
  unless (length ps == length cinfo.fields) $ throwSema
    [ "constructor", errQuote c, "has arity", p . length $ cinfo.fields ]
    [ (r, This ["used with arity", p $ length ps]) ]
  ty <- tyInfo2Ty <$> findTypeInfo cinfo.typeOf
  PData (r, ty) ((, ty) <$> c) <$> sequence (Seq.zipWith f cinfo.fields ps)
  where
    f tid p = do
      t <- tyInfo2Ty <$> findTypeInfo tid
      tyCheckPat t p
tyInferPat (POr r p ps)  = do
  p <- tyInferPat p
  let ty = snd $ info p
  POr (r, snd $ info p) p
    <$> mapM (tyCheckPat ty) ps
tyInferPat x@(PInt {})    = pure $ (, TInt) <$> x
tyInferPat x@(PString {}) = pure $ (, TString) <$> x
tyInferPat x              = pure $ (, TUnknown) <$> x

tyCheckPat :: HasCallStack => Type -> Pattern Range -> Sema (Pattern (Range, Type))
tyCheckPat ty p = do
  p' <- tyInferPat p
  case snd $ info p' of
    ty' | ty == ty' -> pure p'
    TUnknown        -> pure $ (_2 .~ ty) <$> p'
    ty'             -> tyErrorExpect "pattern" ty ty' p

tyCheckExprDecl :: HasCallStack => ExprDecl Range -> Sema (ExprDecl (Range, Type))
tyCheckExprDecl x@(ExprDecl _ _ _ps _) = do
  _ps' <- mapM_ tyInferPat _ps
  pure ((,TUnknown) <$> x)

analyze :: HasCallStack => Module -> Sema (Seq (ExprDecl (Range, Type)))
analyze (Module ds) = do
  ds <- (`witherM` ds) \case
    DData _ c xs -> tyCheckDataDecl c xs $> Nothing
    DExpr e -> pure (Just e)
  -- TODO: error on types we haven't seen? or maybe we can do multiple passes...
  -- dumpSema
  mapM tyCheckExprDecl ds

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
p :: (P.Pretty p) => p -> ErrMsg
p = ErrPretty id
