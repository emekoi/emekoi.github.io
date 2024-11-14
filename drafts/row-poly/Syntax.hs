{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

-- TODO: add type abstractions and applications. maybe

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
    , TRow
    , TType (..)
    , Unique
    , VRow
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
    , typeQuote'
    , writeIORef
    , (!!)
    ) where

import Control.Monad
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Reader
import Data.Foldable1             (foldrMap1')
import Data.IORef                 (IORef)
import Data.IORef                 qualified as IORef
import Data.List                  qualified as List
import Data.List.NonEmpty         (NonEmpty)
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Text                  qualified as Text
import Data.Unique
import GHC.Stack
import Prelude                    hiding ((!!))

type Name = Text.Text
type StrictText = Text.Text
type Dbg = HasCallStack

class Display m x where
  display :: x -> m StrictText

readIORef :: MonadIO m => IORef a -> m a
readIORef = liftIO . IORef.readIORef

writeIORef :: MonadIO m => IORef a -> a -> m ()
writeIORef h = liftIO . IORef.writeIORef h

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

data RKind where
  RKType :: RKind
  RKArrow :: [RKind] -> RKind -> RKind
  RKRow :: RKind
  deriving (Eq, Show)

data RType where
  RTCon :: Name -> RType
  RTVar :: Name -> RType
  RTArrow :: [RType] -> RType -> RType
  RTApply :: RType -> [RType] -> RType
  RTRecord :: [(Name, RType)] -> RType
  RTRecordExt :: [(Name, RType)] -> Name -> RType
  RTForall :: Name -> Maybe RKind -> RType -> RType
  deriving (Eq, Show)

data Expr where
  EInt :: Int -> Expr
  EVar :: Name -> Expr
  ECon :: Name -> [Expr] -> Expr
  ELambda :: [(Name, Maybe RType)] -> Expr -> Expr
  ELet :: Name -> Maybe RType -> Expr -> Expr -> Expr
  ELetRec :: Name -> Maybe RType -> Expr -> Expr -> Expr
  EApply :: Expr -> [Expr] -> Expr
  ESelect :: Expr -> Name -> Expr
  ERestrict :: Expr -> Name -> Expr
  ERecord :: [(Name, Expr)] -> Expr
  ERecordExt :: [(Name, Expr)] -> Expr -> Expr
  EAnnot :: Expr -> RType -> Expr
  deriving (Eq, Show)

data KHole where
  KHFull :: Kind -> KHole
  KHEmpty :: KHole
  deriving (Eq)

data Kind where
  KHole :: IORef KHole -> Kind
  KType :: Kind
  KArrow :: [Kind] -> Kind -> Kind
  KRow :: Kind
  deriving (Eq)

data THole where
  THFull :: VType -> THole
  THEmpty :: Name -> Kind -> Level -> Unique -> THole
  deriving (Eq)

type TRow = Map.Map Name (NonEmpty TType)

data TType where
  TTHole :: IORef THole -> TType
  TTCon :: Name -> Kind -> TType
  TTVar :: Index -> Kind -> TType
  TTArrow :: [TType] -> TType -> TType
  TTApply :: TType -> [TType] -> TType
  TTRow :: TRow -> TType
  TTRowExt :: TRow -> TType -> TType
  TTRecord :: TType -> TType
  TTForall :: Name -> Kind -> TType -> TType
  deriving (Eq)

type VRow = Map.Map Name (NonEmpty VType)

data VType where
  VTHole :: IORef THole -> VType
  VTCon :: Name -> Kind -> VType
  VTVar :: Level -> Kind -> VType
  VTArrow :: [VType] -> VType -> VType
  VTApply :: VType -> [VType] -> VType
  VTRow :: VRow -> VType
  VTRowExt :: VRow -> VType -> VType
  VTRecord :: VType -> VType
  VTForall :: Name -> Kind -> [VType] -> TType -> VType
  deriving (Eq)

data Context = Context
  { typeLevel     :: Level
    -- ^ the current level
  , rawTypeNames  :: [Name]
    -- ^ stack of names from binders passed so far (length = typeLevel)
  , typeEnv       :: [VType]
    -- ^ environment for evaluation of types (length = typeLevel)
  , rawTypeLevels :: Map Name (Level, Kind)
    -- ^ map from names to the levels they were declared at
  , rawTypeCons   :: Map Name Kind
    -- ^ map from names to type constructors
  , rawTermTypes  :: Map Name VType
    -- ^ map from variables to types
  , rawTermCons   :: Map Name VType
    -- ^ map from term constructors to types
  }

type Check = ReaderT Context IO

runCheck :: Dbg => Check m -> IO m
runCheck m =
  runReaderT m Context
    { typeLevel     = Level 0
    , rawTypeNames  = mempty
    , typeEnv       = mempty
    , rawTypeLevels = mempty
    , rawTypeCons   = Map.fromList
      [ ("Unit", KType)
      , ("Int", KType)
      , ("Maybe", KArrow [KType] KType)
      ]
    , rawTermTypes  = mempty
    , rawTermCons   = Map.fromList
      [ ("Unit", VTCon "Unit" KType)
      , ("Nothing", VTForall "x" KType [] tMaybe)
      , ("Just", VTForall "x" KType [] (TTArrow [TTVar (Index 0) KType] tMaybe))
      ]
    }
    where
      tMaybe = TTApply (TTCon "Maybe" (KArrow [KType] KType)) [TTVar (Index 0) KType]

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
typeHole x k l = liftIO $ do
  u <- newUnique
  r <- IORef.newIORef (THEmpty x k l u)
  pure $ VTHole r

typeForce :: (Dbg, MonadIO m) => VType -> m VType
typeForce (VTHole h) = go h
  where
    -- NOTE: fully dereference hole, performing path compression
    go h = readIORef h >>= \case
      THFull (VTHole h') -> do
        t <- go h'
        writeIORef h (THFull t)
        pure t
      THFull t  -> pure t
      THEmpty{} -> pure (VTHole h)
-- NOTE: normalize row extensions
typeForce (VTRowExt xs r) = typeForce r >>= \case
  VTRow ys -> pure $ VTRow (Map.unionWith (<>) xs ys)
  VTRowExt ys r -> pure $ VTRowExt (Map.unionWith (<>) xs ys) r
  r -> pure $ VTRowExt xs r
typeForce t = pure t

typeEval :: (Dbg, MonadIO m) => [VType] -> TType -> m VType
typeEval _ (TTHole h)         = typeForce (VTHole h)
typeEval _ (TTCon c k)        = pure $ VTCon c k
typeEval env (TTVar i _)      = pure $ env !! i
typeEval env (TTArrow t1s t2) = VTArrow <$> mapM (typeEval env) t1s <*> typeEval env t2
typeEval env (TTApply t1 t2s) = VTApply <$> typeEval env t1 <*> mapM (typeEval env) t2s
typeEval env (TTRow fs)       = VTRow <$> mapM (mapM (typeEval env)) fs
typeEval env (TTRowExt fs r)  = VTRowExt <$> mapM (mapM (typeEval env)) fs <*> typeEval env r
typeEval env (TTRecord rs)    = VTRecord <$> typeEval env rs
typeEval env (TTForall x k t) = pure $ VTForall x k env t

typeQuote :: (Dbg, MonadIO m) => Level -> VType -> m TType
typeQuote l t = typeForce t >>= \case
  VTHole h -> pure $ TTHole h
  VTCon c k -> pure $ TTCon c k
  VTVar x k -> pure $ TTVar (lvl2idx l x) k
  VTArrow t1s t2 -> TTArrow <$> mapM (typeQuote l) t1s <*> typeQuote l t2
  VTApply t1 t2s -> TTApply <$> typeQuote l t1 <*> mapM (typeQuote l) t2s
  VTRow fs -> TTRow <$> mapM (mapM (typeQuote l)) fs
  VTRowExt fs r -> TTRowExt <$> mapM (mapM (typeQuote l)) fs <*> typeQuote l r
  VTRecord rs -> TTRecord <$> typeQuote l rs
  VTForall x k env t -> do
    t' <- typeEval (VTVar l k : env) t
    TTForall x k <$> typeQuote (succ l) t'

typeQuote' :: (Dbg, MonadIO m) => Level -> Level -> VType -> m TType
typeQuote' b l t = typeForce t >>= \case
  VTHole h -> do
    -- NOTE: we clamp all levels to b
    readIORef h >>= \case
      THEmpty n k l u | l > b ->
        writeIORef h (THEmpty n k b u)
      _ -> pure ()
    pure $ TTHole h
  VTCon c k -> pure $ TTCon c k
  VTVar x k -> pure $ TTVar (lvl2idx l x) k
  VTArrow t1s t2 -> TTArrow <$> mapM (typeQuote' b l) t1s <*> typeQuote' b l t2
  VTApply t1 t2s -> TTApply <$> typeQuote' b l t1 <*> mapM (typeQuote' b l) t2s
  VTRow fs -> TTRow <$> mapM (mapM (typeQuote' b l)) fs
  VTRowExt fs r -> TTRowExt <$> mapM (mapM (typeQuote' b l)) fs <*> typeQuote' b l r
  VTRecord rs -> TTRecord <$> typeQuote' b l rs
  VTForall x k env t -> do
    t' <- typeEval (VTVar l k : env) t
    TTForall x k <$> typeQuote' b (succ l) t'

instance (MonadIO m) => Display m Kind where
  display = go False
    where
      parens True x  = "(" <> x <> ")"
      parens False x = x

      go p (KHole h) = readIORef h >>= \case
        KHFull k -> go p k
        KHEmpty -> pure "?"
      go _ KType = pure "Type"
      go p (KArrow k1s k2) = do
        k1s <- mapM (go True) k1s
        k2 <- go False k2
        pure $ parens p (Text.intercalate " -> " (k1s ++ [k2]))
      go _ KRow = pure "Row"

data TTypeParen
  = ParenNone
  | ParenArrow
  | ParenApply

instance Display Check TType where
  display = go ParenNone
    where
      displayUnique = Text.pack . show . hashUnique

      parenApply ParenApply x = "(" <> x <> ")"
      parenApply _ x          = x

      parens ParenNone x = x
      parens _ x         = "(" <> x <> ")"

      field (x, ts) = do
        let f t = x <> " : " <> t
        ts <- mapM (go ParenNone) ts
        pure $ foldrMap1' f (\x r -> f x <> ", " <> r) ts

      go p (TTHole h) = readIORef h >>= \case
        THFull t -> do
          l <- asks typeLevel
          typeQuote l t >>= go p
        THEmpty x k _ u -> do
          k <- display k
          pure $ Text.cons '?' k <> displayUnique u <> "." <> x
      go _ (TTCon c _) = pure c
      go _ (TTVar i _) = (!! i) <$> asks rawTypeNames
      go p (TTArrow t1s t2) = do
        t1s <- mapM (go ParenArrow) t1s
        t2 <- go ParenNone t2
        pure $ parens p (Text.intercalate " -> " (t1s ++ [t2]))
      go p (TTApply t1 t2s) = do
        t1 <- go ParenNone t1
        t2s <- mapM (go ParenApply) t2s
        pure $ parenApply p (Text.intercalate " " (t1 : t2s))
      go _ (TTRow (Map.toList -> fs)) = do
        Text.intercalate ", " <$> mapM field fs
      go _ (TTRowExt (Map.toList -> fs) r) = do
        fs <- Text.intercalate ", " <$> mapM field fs
        r <- go ParenNone r
        pure $ if Text.null fs then r else fs <> " | " <> r
      go _ (TTRecord fs) = do
        fs <- go ParenNone fs
        pure $ "{" <> fs <>  "}"
      go p (TTForall x k t) = do
        t <- typeBind x k $ go ParenNone t
        x <- kindForce k >>= \k -> do
          k <- display k
          pure $ "(" <> x <> " : " <> k <>  ")"
        pure $ parens p ("forall " <> x <> ". " <> t)

instance Display Check VType where
  display t = do
    l <- asks typeLevel
    typeQuote l t >>= display
