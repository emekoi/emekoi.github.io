{-# OPTIONS_GHC -Wno-unused-top-binds   #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RecordWildCards            #-}

module Sema
    ( Sema
    , runSema
    , Type (..)
    , analyze
    ) where

import Control.Category     ((>>>))
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Bifunctor
import Data.Functor
import Data.IntMap.Strict   (IntMap)
import Data.IntMap.Strict   qualified as IntMap
import Data.IORef           (IORef)
import Data.IORef           qualified as IORef
import Data.Map.Strict      (Map)
import Data.Map.Strict      qualified as Map
import Data.Sequence        (Seq (..))
import Data.Sequence        qualified as Seq
import Data.Text            (Text)
import Error
import GHC.Stack
import Parser               hiding (Type)
import Prettyprinter        qualified as P
import Witherable

newIORef :: (MonadIO m) => x -> m (IORef x)
newIORef = liftIO . IORef.newIORef

readIORef :: (MonadIO m) => IORef x -> m x
readIORef = liftIO . IORef.readIORef

writeIORef :: (MonadIO m) => IORef x -> x -> m ()
writeIORef r = liftIO . IORef.writeIORef r

-- modifyIORef' :: (MonadIO m) => IORef x -> (x -> x) -> m ()
-- modifyIORef' k = liftIO . IORef.modifyIORef k

modifyIORef :: (MonadIO m) => IORef x -> (x -> x) -> m ()
modifyIORef k = liftIO . IORef.modifyIORef' k

newtype TypeId
  = TypeId Int
  deriving (Eq, Num, Ord, Show)

newtype ConId
  = ConId Int
  deriving (Eq, Num, Ord, Show)

newtype TermId
  = TermId Int
  deriving (Eq, Num, Ord, Show)

newtype NoShow a
  = NoShow a
  deriving (Eq, Functor, Ord)

instance Show (NoShow a) where
  show _ = "<omitted>"

data Type
  = TUnknown Int (NoShow (IORef (Maybe Type)))
  | TData Text TypeId
  deriving (Eq, Show)

pattern TInt :: Type
pattern TInt = TData "Int" (-1)

pattern TString :: Type
pattern TString = TData "String" (-2)

instance P.Pretty Type where
  pretty (TUnknown i _) = "?" <> P.pretty i
  pretty (TData c _)    = P.pretty c

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
  , termNames   :: IORef (Map Text TermId)
  , termLevel   :: Int
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
    <*> newIORef mempty
    <*> pure 0
  runReaderT s ctx
  where
    initTypes = Map.fromList []
    initTypeInfo = IntMap.fromList []

fresh :: Sema Int
fresh = ask >>= \s -> do
  fresh <- readIORef s.fresh
  modifyIORef s.fresh succ
  pure fresh

freshMeta :: Sema Type
freshMeta = do
  id <- fresh
  m <- newIORef Nothing
  pure $ TUnknown id (NoShow m)

tyUnify :: Type -> Type -> Sema Bool
tyUnify ty ty' | ty == ty' = pure True
tyUnify (TUnknown _ (NoShow ty)) ty' = do
  readIORef ty >>= \case
    Just ty -> tyUnify ty ty'
    Nothing ->
      -- TODO: occurs check
      writeIORef ty (Just ty') $> True
tyUnify ty ty'@(TUnknown {}) = tyUnify ty' ty
tyUnify _ _ = pure False

tyErrorExpect
  :: (HasRange r, P.Pretty a2, P.Pretty a3)
  => ErrMsg -> a2 -> a3 -> r -> Sema a1
tyErrorExpect thing expect got r =
  throwSema [ "expected", thing, "of type", errQuote expect ]
    [ (range r, This [
        thing, "has type", errQuote got
      ])
    ]

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

makeTypeId :: Bool -> Name Range -> Sema TypeId
makeTypeId m (Name _ n) = do
  SemaState{types} <- ask
  readIORef types >>= Map.lookup n .- \case
    Just id -> pure id
    Nothing -> do
      id <- TypeId . f <$> fresh
      modifyIORef types (Map.insert n id)
      pure id
  where
    f n | m = negate (n + 1)
    f n = n

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

tyCheckDataDecl :: HasCallStack => Name Range -> Seq (DataCon Range) -> Sema ()
tyCheckDataDecl t cs = do
  tid@(TypeId id) <- findTypeId t
  SemaState{typeInfo,dataConInfo} <- ask
  readIORef typeInfo >>= IntMap.lookup id .- \case
    Just info -> do
      throwSema
        [ "redefinition of",if id < 0 then "builtin" else "", "type", errQuote t ]
        [ (range info.name, This "first defined here")
        , (range t, This "redefined here")
        ]
    Nothing -> modifyIORef typeInfo . IntMap.insert id $
      TypeInfo { name = t, id = tid, span = length cs }
  forM_ cs \(DataCon _ name xs) -> do
    id@(ConId cid) <- makeConId name
    fields <- mapM findTypeId xs
    modifyIORef dataConInfo . IntMap.insert cid $
      ConInfo { typeOf = tid, .. }

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
    p = ErrPretty id

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
tyInferPat x              = do
  ty <- freshMeta
  pure $ (, ty) <$> x

tyCheckPat :: HasCallStack => Type -> Pattern Range -> Sema (Pattern (Range, Type))
tyCheckPat ty p = do
  p' <- tyInferPat p
  let ty' = snd $ info p'
  u <- tyUnify ty ty'
  unless u $
    tyErrorExpect "pattern" ty ty' p
  pure p'

tyInferExpr :: HasCallStack => Expr Range -> Sema (Expr (Range, Type))
tyInferExpr = error "TODO"

tyCheckExprDecl :: HasCallStack => ExprDecl Range -> Sema (ExprDecl (Range, Type))
tyCheckExprDecl (ExprDecl r f ps e) = do
  ps <- mapM tyInferPat ps
  e <- tyInferExpr e
  let ty = snd $ info e
  pure $ ExprDecl (r, ty) ((,ty) <$> f) ps e

analyze :: HasCallStack => Module -> Sema (Seq (ExprDecl (Range, Type)))
analyze (Module ds) = do
  forM_ ds \case
    DData _ c m _ -> makeTypeId m c $> ()
    DExpr _ -> pure ()
  ds <- (`witherM` ds) \case
    DData _ c _ xs -> tyCheckDataDecl c xs $> Nothing
    DExpr e -> pure (Just e)
  mapM tyCheckExprDecl ds
