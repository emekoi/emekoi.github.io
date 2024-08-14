{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module Syntax
    ( Check
    , Context (..)
    , Dbg
    , Expr (..)
    , Hole (..)
    , Index (..)
    , Level (..)
    , Name
    , RType (..)
    , StrictText
    , TType (..)
    , Unique (..)
    , VType (..)
    , exprBind
    , holeNew
    , holeRead
    , holeWrite
    , lvl2idx
    , runCheck
    , typeBind
    , typeEval
    , typeForce
    , typePrint
    , typePrint'
    , typeQuote
    , uniqueNew
    , (!!)
    ) where

import Control.Monad
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Reader
import Data.IORef                 (IORef)
import Data.IORef                 qualified as IORef
import Data.List                  qualified as List
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Text                  qualified as Text
import GHC.Stack
import Prelude                    hiding ((!!))

type Name = Text.Text
type StrictText = Text.Text
type Dbg = HasCallStack

newtype Unique
  = Unique { unUnique :: Int }
  deriving (Eq, Ord, Show)

newtype Level
  = Level { unLevel :: Int }
  deriving (Bounded, Enum, Eq, Ord, Show)

newtype Index
  = Index { unIndex :: Int }
  deriving (Bounded, Enum, Eq, Ord, Show)

lvl2idx :: Level -> Level -> Index
lvl2idx (Level l) (Level x) = Index (l - x - 1)

(!!) :: Dbg => [a] -> Index -> a
xs !! (Index i) = xs List.!! i

data RType where
  RTVar :: Name -> RType
  RTCon :: Name -> RType
  RTArrow :: RType -> RType -> RType
  RTForall :: Name -> RType -> RType
  deriving (Eq, Show)

data Hole where
  Empty :: Name -> Unique -> Level -> Hole
  Full :: VType -> Hole
  deriving (Eq)

data TType where
  TTVar :: Index -> TType
  TTCon :: Name -> Unique -> TType
  TTArrow :: TType -> TType -> TType
  TTForall :: Name -> TType -> TType
  TTHole :: IORef Hole -> TType
  deriving (Eq)

data VType where
  VTVar :: Level -> VType
  VTCon :: Name -> Unique -> VType
  VTArrow :: VType -> VType -> VType
  VTForall :: Name -> [VType] -> TType -> VType
  VTHole :: IORef Hole -> VType
  deriving (Eq)

data Expr where
  EUnit :: Expr
  EVar :: Name -> Expr
  EApply :: Expr -> Expr -> Expr
  ELambda :: Name -> Maybe RType -> Expr -> Expr
  ELet :: Name -> Maybe RType -> Expr -> Expr -> Expr
  EAnnot :: Expr -> RType -> Expr
  deriving (Eq, Show)

data Context = Context
  { unique        :: IORef Int
  , typeLevel     :: Level
  , rawTypeLevels :: Map Name Level
  , rawTypeNames  :: [Name]
  , typeEnv       :: [VType]
  , rawTermTypes  :: Map Name VType
  }

type Check = ReaderT Context IO

runCheck :: Dbg => Check m -> IO m
runCheck m = do
  unique <- IORef.newIORef 0
  runReaderT m Context
    { unique        = unique
    , typeLevel     = Level 0
    , rawTypeLevels = mempty
    , rawTypeNames  = mempty
    , typeEnv       = mempty
    , rawTermTypes  = mempty
    }

typeBind :: Dbg => Name -> Check r -> Check r
typeBind x = local \Context{..} -> Context
  { typeLevel     = succ typeLevel
  , rawTypeLevels = Map.insert x typeLevel rawTypeLevels
  , rawTypeNames  = x : rawTypeNames
  , typeEnv       = VTVar typeLevel : typeEnv
  , ..
  }

exprBind :: Dbg => Name -> VType -> Check r -> Check r
exprBind x t = local \ctx -> ctx
  { rawTermTypes = Map.insert x t (rawTermTypes ctx)
  }

uniqueNew :: Check Unique
uniqueNew = do
  uniqueSource <- asks unique
  liftIO $ IORef.atomicModifyIORef' uniqueSource \i ->
    let !z = i + 1 in (z, Unique z)

holeNew :: Name -> Level -> Check VType
holeNew x l = do
  u <- uniqueNew
  liftIO $ VTHole <$> IORef.newIORef (Empty x u l)

holeRead :: MonadIO m => IORef Hole -> m Hole
holeRead = liftIO . IORef.readIORef

holeWrite :: MonadIO m => IORef Hole -> Hole -> m ()
holeWrite h = liftIO . IORef.writeIORef h

typeForce :: (Dbg, MonadIO m) => VType -> m VType
typeForce (VTHole h) = go h
  where
    go h = holeRead h >>= \case
      Full (VTHole h')  -> do
        t <- go h'
        holeWrite h (Full t)
        pure t
      Full t  -> pure t
      Empty{} -> pure (VTHole h)
typeForce t = pure t

typeEval :: (Dbg, MonadIO m) => [VType] -> TType -> m VType
typeEval env (TTVar i)      = pure $ env !! i
typeEval _ (TTCon c i)      = pure $ VTCon c i
typeEval env (TTArrow a b)  = VTArrow <$> typeEval env a <*> typeEval env b
typeEval env (TTForall x t) = pure $ VTForall x env t
typeEval _ (TTHole h)       = typeForce (VTHole h)

typeQuote :: (Dbg, MonadIO m) => Level -> VType -> m TType
typeQuote l t = typeForce t >>= \case
  VTVar x -> pure $ TTVar (lvl2idx l x)
  VTCon c i -> pure $ TTCon c i
  VTArrow s t -> TTArrow <$> typeQuote l s <*> typeQuote l t
  VTForall x env t -> do
    t' <- typeEval (VTVar l : env) t
    TTForall x <$> typeQuote (succ l) t'
  VTHole h -> pure $ TTHole h

-- _typeQuote' :: (Dbg, MonadIO m) => Level -> Level -> VType -> m TType
-- _typeQuote' b l t = typeForce t >>= \case
--   VTVar x -> pure $ TTVar (lvl2idx l x)
--   VTCon c i -> pure $ TTCon c i
--   VTArrow s t -> TTArrow
--     <$> _typeQuote' b l s
--     <*> _typeQuote' b l t
--   VTForall x env t -> do
--     t' <- typeEval (VTVar l : env) t
--     TTForall x <$> _typeQuote' b (succ l) t'
--   VTHole h -> do
--     liftIO $ readIORef h >>= \case
--       Empty n x l | l > b ->
--         writeIORef h (Empty n x b)
--       _ -> pure ()
--     pure $ TTHole h


typePrint' :: Dbg => TType -> Check StrictText
typePrint' t = do
  go False t
  where
    parens True x  = "(" <> x <> ")"
    parens False x = x

    go _ (TTVar i) = do
      x <- (!! i) <$> asks rawTypeNames
      pure $ x <> "@" <> Text.pack (show (unIndex i))
    go _ (TTCon c _) = pure c
    go p (TTArrow a b) = do
      a <- go True a
      b <- go False b
      pure $ parens p (a <> " -> " <> b)
    go p (TTForall x t) = do
      t <- typeBind x $ go False t
      pure $ parens p ("forall " <> x <> ". " <> t)
    go p (TTHole h) = holeRead h >>= \case
      Empty x u l -> pure $ "?" <> x <> "." <> Text.pack (show (unUnique u) <> "@" <> show (unLevel l))
      Full t -> do
        l <- asks typeLevel
        t <- typeQuote l t
        go p t

typePrint :: Dbg => VType -> Check StrictText
typePrint t = do
  l <- asks typeLevel
  go l False t
  where
    parens True x  = "(" <> x <> ")"
    parens False x = x

    go l p t = typeForce t >>= \case
      VTVar l' -> do
        x <- (!! lvl2idx l l') <$> asks rawTypeNames
        pure $ x <> "@" <> Text.pack (show (unLevel l'))
      VTCon c _ -> pure c
      VTArrow a b -> do
        a <- go l True a
        b <- go l False b
        pure $ parens p (a <> " -> " <> b)
      VTForall x env t -> do
        t <- typeEval (VTVar l : env) t
          >>= typeBind x . go (succ l) False
        pure $ parens p ("forall " <> x <> ". " <> t)
      VTHole h -> holeRead h >>= \case
        Empty x u l -> pure $ "?" <> x <> "." <> Text.pack (show (unUnique u) <> "@" <> show (unLevel l))
        Full t -> go l p t
