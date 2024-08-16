{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module Syntax
    ( Check
    , Context (..)
    , Dbg
    , Display (..)
    , Expr (..)
    , Index (..)
    , KHole (..)
    , Kind (..)
    , Level (..)
    , Name
    , RKind (..)
    , RType (..)
    , StrictText
    , THole (..)
    , TType (..)
    , Unique (..)
    , VType (..)
    , exprBind
    , kindForce
    , kindHole
    , lvl2idx
    , readIORef
    , runCheck
    , typeBind
    , typeEval
    , typeForce
    , typeHole
    , typeQuote
    , uniqueNew
    , writeIORef
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

class Display m x where
  display :: x -> m StrictText

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

readIORef :: MonadIO m => IORef a -> m a
readIORef = liftIO . IORef.readIORef

writeIORef :: MonadIO m => IORef a -> a -> m ()
writeIORef h = liftIO . IORef.writeIORef h

data RKind where
  RKRow :: RKind
  RKType :: RKind
  RKArrow :: RKind -> RKind -> RKind
  deriving (Eq, Show)

data RType where
  RTForall :: Name -> Maybe RKind -> RType -> RType
  RTVar :: Name -> RType
  RTCon :: Name -> RType
  RTArrow :: RType -> RType -> RType
  RTApply :: RType -> RType -> RType
  RREmpty :: RType
  RRExtend :: Name -> RType -> RType -> RType
  RRRecord :: RType -> RType
  deriving (Eq, Show)

data KHole where
  KHEmpty :: KHole
  KHFull :: Kind -> KHole
  deriving (Eq)

data Kind where
  KType :: Kind
  KRow :: Kind
  KArrow :: Kind -> Kind -> Kind
  KHole :: IORef KHole -> Kind
  deriving (Eq)

data THole where
  THEmpty :: Name -> Kind -> Level -> THole
  THFull :: VType -> THole
  deriving (Eq)

data TType where
  TTForall :: Name -> Kind -> TType -> TType
  TTVar :: Index -> Kind -> TType
  TTCon :: Name -> Kind -> Unique -> TType
  TTArrow :: TType -> TType -> TType
  TTApply :: TType -> TType -> TType
  TREmpty :: TType
  TRExtend :: Name -> TType -> TType -> TType
  TRRecord :: TType -> TType
  TTHole :: IORef THole -> TType
  deriving (Eq)

data VType where
  VTForall :: Name -> Kind -> [VType] -> TType -> VType
  VTVar :: Level -> Kind -> VType
  VTCon :: Name -> Kind -> Unique -> VType
  VTArrow :: VType -> VType -> VType
  VTApply :: VType -> VType -> VType
  VREmpty :: VType
  VRExtend :: Name -> VType -> VType -> VType
  VRRecord :: VType -> VType
  VTHole :: IORef THole -> VType
  deriving (Eq)

data Expr where
  EUnit :: Expr
  EInt :: Int -> Expr
  EVar :: Name -> Expr
  EApply :: Expr -> Expr -> Expr
  ELambda :: Name -> Maybe RType -> Expr -> Expr
  ELet :: Name -> Maybe RType -> Expr -> Expr -> Expr
  ELetRec :: Name -> Maybe RType -> Expr -> Expr -> Expr
  EEmpty :: Expr
  ESelect :: Expr -> Name -> Expr
  ERestrict :: Expr -> Name -> Expr
  EExtend :: Name -> Expr -> Expr -> Expr
  EAnnot :: Expr -> RType -> Expr
  deriving (Eq, Show)

data Context = Context
  { unique        :: IORef Int
    -- ^ generation of unique ids
  , typeLevel     :: Level
    -- ^ the current level
  , rawTypeNames  :: [Name]
    -- ^ stack of names from binders passed so far (length = typeLevel)
  , typeEnv       :: [VType]
    -- ^ environment for evaluation of types (length = typeLevel)
  , rawTypeLevels :: Map Name (Level, Kind)
    -- ^ map from names to the levels they were declared at
  , rawTypeCons   :: Map Name (Unique, Kind)
    -- ^ map from names to type constructors
  , rawTermTypes  :: Map Name VType
    -- ^ map from variables to types
  }

type Check = ReaderT Context IO

runCheck :: Dbg => Check m -> IO m
runCheck m = do
  unique <- IORef.newIORef 2

  runReaderT m Context
    { unique        = unique
    , typeLevel     = Level 0
    , rawTypeNames  = mempty
    , typeEnv       = mempty
    , rawTypeLevels = mempty
    , rawTypeCons   = Map.fromList
      [ ("Unit", (Unique 0, KType))
      , ("Int", (Unique 1, KType))
      ]
    , rawTermTypes  = mempty
    }

typeBind :: Dbg => Name -> Kind -> Check r -> Check r
typeBind x k = local \Context{..} -> Context
  { typeLevel     = succ typeLevel
  , rawTypeLevels = Map.insert x (typeLevel, k) rawTypeLevels
  , rawTypeNames  = x : rawTypeNames
  -- NOTE: since we've bound the variable, it should eval to itself
  , typeEnv       = VTVar typeLevel k : typeEnv
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

kindHole :: Check Kind
kindHole = liftIO $ KHole <$> IORef.newIORef KHEmpty

kindForce :: (Dbg, MonadIO m) => Kind -> m Kind
kindForce (KHole h) = go h
  where
    -- NOTE: fully dereference hole, performing path compression
    go h = readIORef h >>= \case
      KHFull (KHole h')  -> do
        t <- go h'
        writeIORef h (KHFull t)
        pure t
      KHFull t  -> pure t
      KHEmpty{} -> pure (KHole h)
kindForce t = pure t

typeHole :: Name -> Kind -> Level -> Check VType
typeHole x k l = liftIO $
  VTHole <$> IORef.newIORef (THEmpty x k l)

typeForce :: (Dbg, MonadIO m) => VType -> m VType
typeForce (VTHole h) = go h
  where
    -- NOTE: fully dereference hole, performing path compression
    go h = readIORef h >>= \case
      THFull (VTHole h')  -> do
        t <- go h'
        writeIORef h (THFull t)
        pure t
      THFull t  -> pure t
      THEmpty{} -> pure (VTHole h)
typeForce t = pure t

typeEval :: (Dbg, MonadIO m) => [VType] -> TType -> m VType
typeEval env (TTForall x k t) = pure $ VTForall x k env t
typeEval env (TTVar i _)      = pure $ env !! i
typeEval _ (TTCon c k u)      = pure $ VTCon c k u
typeEval env (TTArrow a b)    = VTArrow <$> typeEval env a <*> typeEval env b
typeEval env (TTApply a b)    = VTApply <$> typeEval env a <*> typeEval env b
typeEval _ TREmpty            = pure VREmpty
typeEval env (TRExtend x t r) = VRExtend x <$> typeEval env t <*> typeEval env r
typeEval env (TRRecord r)     = VRRecord <$> typeEval env r
typeEval _ (TTHole h)         = typeForce (VTHole h)

typeQuote :: (Dbg, MonadIO m) => Level -> VType -> m TType
typeQuote l t = typeForce t >>= \case
  VTForall x k env t -> do
    t' <- typeEval (VTVar l k : env) t
    TTForall x k <$> typeQuote (succ l) t'
  VTVar x k -> pure $ TTVar (lvl2idx l x) k
  VTCon c k u -> pure $ TTCon c k u
  VTArrow s t -> TTArrow <$> typeQuote l s <*> typeQuote l t
  VTApply s t -> TTApply <$> typeQuote l s <*> typeQuote l t
  VREmpty -> pure TREmpty
  VRExtend x t r -> TRExtend x <$> typeQuote l t <*> typeQuote l r
  VRRecord r -> TRRecord <$> typeQuote l r
  VTHole h -> pure $ TTHole h

-- typeQuote' :: (Dbg, MonadIO m) => Level -> Level -> VType -> m TType
-- typeQuote' b l t = typeForce t >>= \case
--   VTVar x k -> pure $ TTVar (lvl2idx l x) k
--   VTCon c k u -> pure $ TTCon c k u
--   VTArrow s t -> TTArrow
--     <$> typeQuote' b l s
--     <*> typeQuote' b l t
--   VTApply s t -> TTApply
--     <$> typeQuote' b l s
--     <*> typeQuote' b l t
--   VTForall x k env t -> do
--     t' <- typeEval (VTVar l k : env) t
--     TTForall x k <$> typeQuote' b (succ l) t'
--   VTHole h -> do
--     -- NOTE: we clamp all levels to b
--     readIORef h >>= \case
--       THEmpty n k l | l > b ->
--         writeIORef h (THEmpty n k b)
--       _ -> pure ()
--     pure $ TTHole h

instance (MonadIO m) => Display m Kind where
  display = go False
    where
      parens True x  = "(" <> x <> ")"
      parens False x = x

      go _ KType = pure "Type"
      go _ KRow = pure "Row"
      go p (KArrow a b) = do
        a <- go True a
        b <- go False b
        pure $ parens p (a <> " -> " <> b)
      go p (KHole h) = readIORef h >>= \case
        KHEmpty -> pure "?"
        KHFull k -> go p k

instance Display Check TType where
  display = go False False
    where
      parens True x  = "(" <> x <> ")"
      parens False x = x

      go p1 p2 (TTForall x k t) = do
        t <- typeBind x k $ go False False t
        x <- kindForce k >>= \k -> do
          k <- display k
          pure $ "(" <> x <> " : " <> k <>  ")"
        pure $ parens (p1 || p2) ("forall " <> x <> ". " <> t)
      go _ _ (TTVar i _) = (!! i) <$> asks rawTypeNames
      go _ _ (TTCon c _ _) = pure c
      go p1 p2 (TTArrow a b) = do
        a <- go True False a
        b <- go False False b
        pure $ parens (p1 || p2) (a <> " -> " <> b)
      go _ p2 (TTApply a b) = do
        a <- go True False a
        b <- go False True b
        pure $ parens p2 (a <> " " <> b)
      go _ _ TREmpty = pure "<>"
      go _ _ (TRExtend x t r) = do
        t <- go False False t
        r <- go False False r
        pure $ "<" <> x <> " : " <> t <> " | " <> r <> ">"
      go _ _ (TRRecord r) = do
        r <- go False False r
        pure $ "{" <> r <> "}"
      go p1 p2 (TTHole h) = readIORef h >>= \case
        THEmpty x k l -> do
          k <- display k
          pure $ Text.cons '?' k <> Text.pack (show $ unLevel l) <> "." <> x
        THFull t -> do
          l <- asks typeLevel
          t <- typeQuote l t
          go p1 p2 t

instance Display Check VType where
  display t = do
    l <- asks typeLevel
    typeQuote l t >>= display
    -- go l False t
    -- where
    --   parens True x  = "(" <> x <> ")"
    --   parens False x = x

    --   go l p t = typeForce t >>= \case
    --     VTVar l'_  -> do
    --       x <- (!! lvl2idx l l') <$> asks rawTypeNames
    --       pure $ x <> "@" <> Text.pack (show (unLevel l'))
    --     VTCon c _ _ -> pure c
    --     VTArrow a b -> do
    --       a <- go l True a
    --       b <- go l False b
    --       pure $ parens p (a <> " -> " <> b)
    --     VTForall x env t -> do
    --       t <- typeEval (VTVar l : env) t
    --         >>= typeBind x . go (succ l) False
    --       pure $ parens p ("forall " <> x <> ". " <> t)
    --     VTHole h -> readIORef h >>= \case
    --       Empty x u l -> pure $ "?" <> x <> "."
    --         <> Text.pack (show (unUnique u) <> "@" <> show (unLevel l))
