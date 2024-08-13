{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FieldSelectors      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}

#if defined(POLY_SUB)
#define TYPE_UNIFY typeSub
#else
#define TYPE_UNIFY typeUnify
#endif

module Poly where

import Control.Exception          (Exception (..), throwIO)
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Reader
import Data.Functor               (($>))
import Data.IORef                 (IORef, atomicModifyIORef', newIORef,
                                   readIORef, writeIORef)
import Data.List                  qualified as List
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Typeable              (Typeable)
import GHC.Stack.Types            (HasCallStack)
import Prelude                    hiding ((!!))

type Dbg = HasCallStack

bind2 :: Monad m => m a -> m b -> (a -> b -> m c) -> m c
bind2 x y k = x >>= ((y >>=) . k)

throw :: (Dbg, Exception e, MonadIO m) => e -> m a
throw = liftIO . throwIO

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
  RTVar :: String -> RType
  RTCon :: String -> RType
  RTArrow :: RType -> RType -> RType
  RTForall :: String -> RType -> RType
  deriving (Eq, Show)

data Hole where
  Empty :: String -> Unique -> Level -> Hole
  Full :: VType -> Hole
  deriving (Eq)

data TType where
  TTVar :: Index -> TType
  TTCon :: String -> Unique -> TType
  TTArrow :: TType -> TType -> TType
  TTForall :: String -> TType -> TType
  TTHole :: IORef Hole -> TType
  deriving (Eq)

data VType where
  VTVar :: Level -> VType
  VTCon :: String -> Unique -> VType
  VTArrow :: VType -> VType -> VType
  VTForall :: String -> [VType] -> TType -> VType
  VTHole :: IORef Hole -> VType
  deriving (Eq)

data Expr where
  EVar :: String -> Expr
  EApply :: Expr -> Expr -> Expr
  ELambda :: String -> Maybe RType -> Expr -> Expr
  ELet :: String -> Maybe RType -> Expr -> Expr -> Expr
  EAnnot :: Expr -> RType -> Expr
  deriving (Eq, Show)

data TypeError
  = TypeErrorMismatch String String
  | TypeErrorOccursCheck String String
  | TypeErrorTypeVariableEscape String
  | TypeErrorTypeVariableUnbound String
  | TypeErrorVariableUnbound String
  | TypeErrorNonFunction Expr String
  deriving (Show, Typeable)

instance Exception TypeError where

data Context = Context
  { unique        :: IORef Int
  , typeLevel     :: Level
  , rawTypeLevels :: Map String Level
  , rawTypeNames  :: [String]
  , typeEnv       :: [VType]
  , rawTermTypes  :: Map String VType
  }

type Check = ReaderT Context IO

runCheck :: Dbg => Check m -> IO m
runCheck m = do
  unique <- newIORef 0
  runReaderT m Context
    { unique        = unique
    , typeLevel     = Level 0
    , rawTypeLevels = mempty
    , rawTypeNames  = mempty
    , typeEnv       = mempty
    , rawTermTypes  = mempty
    }

uniqueNew :: Check Unique
uniqueNew = do
  uniqueSource <- asks unique
  liftIO $ atomicModifyIORef' uniqueSource \i ->
    let !z = i + 1 in (z, Unique z)

typeForce :: MonadIO m => VType -> m VType
typeForce (VTHole h) = liftIO $ go h
  where
    go h = readIORef h >>= \case
      Full (VTHole h')  -> do
        t <- go h'
        writeIORef h (Full t)
        pure t
      Full t  -> pure t
      Empty{} -> pure (VTHole h)
typeForce t = pure t

typeEval :: (Dbg, MonadIO m) => [VType] -> TType -> m VType
typeEval env (TTVar i)      = pure $ env !! i
typeEval _ (TTCon c i)      = pure $ VTCon c i
typeEval env (TTArrow a b)  = VTArrow <$> typeEval env a <*> typeEval env b
typeEval env (TTForall x t) = pure $ VTForall x env t
-- typeEval _ (TTHole h)       = liftIO (readIORef h) >>= \case
--   Empty{} -> pure (VTHole h)
--   Full t  -> pure t
typeEval _ (TTHole h)       = typeForce (VTHole h)

typeQuote :: MonadIO m => Level -> VType -> m TType
typeQuote l t = typeForce t >>= \case
  VTVar x -> pure $ TTVar (lvl2idx l x)
  VTCon c i -> pure $ TTCon c i
  VTArrow s t -> TTArrow <$> typeQuote l s <*> typeQuote l t
  VTForall x env t -> do
    t' <- typeEval (VTVar l : env) t
    TTForall x <$> typeQuote (succ l) t'
  VTHole h -> pure $ TTHole h

typeQuote' :: MonadIO m => Level -> Level -> VType -> m TType
typeQuote' b l t = typeForce t >>= \case
  VTVar x -> pure $ TTVar (lvl2idx l x)
  VTCon c i -> pure $ TTCon c i
  VTArrow s t -> TTArrow
    <$> typeQuote' b l s
    <*> typeQuote' b l t
  VTForall x env t -> do
    t' <- typeEval (VTVar l : env) t
    TTForall x <$> typeQuote' b (succ l) t'
  VTHole h -> do
    liftIO $ readIORef h >>= \case
      Empty n x l | l > b ->
        writeIORef h (Empty n x b)
      _ -> pure ()
    pure $ TTHole h

typeHole :: String -> Level -> Check VType
typeHole x l = do
  u <- uniqueNew
  liftIO $ VTHole <$> newIORef (Empty x u l)

typeSub :: VType -> VType -> Check ()
typeSub = error "TODO"

-- typeMetaSolve :: IORef MetaVar -> Type -> Check ()
-- typeMetaSolve m (TyArrow a b) = do
--   typeMetaSolve m a
--   typeMetaSolve m b
-- typeMetaSolve m (TyApply a b) = do
--   typeMetaSolve m a
--   typeMetaSolve m b
-- typeMetaSolve m (TyMeta m') = do
--   when (m == m') $ do
--     sM <- typeDisplay (TyMeta m)
--     sM' <- typeDisplay (TyMeta m')
--     checkThrow (ErrTypeOccursCheck sM sM')
--   liftIO (readIORef m') >>= \case
--     Solved t      -> typeMetaSolve m t
--     Unsolved x l' -> liftIO (readIORef m) >>= \case
--       Solved{}     -> pure ()
--       Unsolved _ l -> liftIO $
--         writeIORef m' (Unsolved x (min l l'))
-- typeMetaSolve _ _            = pure ()

typeUnify :: Dbg => VType -> VType -> Check ()
typeUnify t1 t2 = bind2 (typeForce t1) (typeForce t2) \cases
  _ _ -> pure ()
--   (TyMeta t1) (TyMeta t2) -> pure ()
--   (TyMeta t1) t2 -> pure ()
--   t1 (TyMeta t2) -> pure ()
--   (TyCon i1 k1 _) (TyCon i2 k2 _) | i1 == i2 && k1 == k2 -> pure ()
--   (TyArrow t1 t2) (TyArrow t1' t2') ->
--     typeUnify t1 t1' *> typeUnify t2 t2'
--   (TyApply t1 t2) (TyApply t1' t2') ->
--     typeUnify t1 t1' *> typeUnify t2 t2'
--   _ _ -> do
--     s1 <- typeDisplay t1
--     s2 <- typeDisplay t2
--     checkThrow (ErrTypeMismatch s1 s2)

-- -- typeUnify
-- -- typeUnify t1 t2 = do
-- --   t1' <- readMeta t1
-- --   t2' <- readMeta t2
-- --   case (t1', t2') of
-- --     (Just (Solved t1, _), _) -> typeUnify t1 t2
-- --     (_, Just (Solved t2, _)) -> typeUnify t1 t2
-- --     (Just (_, u), _)         -> occurs u t2 *> writeIORef u (Solved t2)
-- --     (_, Just (_, u))         -> occurs u t1 *> writeIORef u (Solved t1)
-- --     _                        -> throwIO TypeUnifyError
--   where
--     bind2 x y k = x >>= ((y >>=) . k)

typeBind :: Dbg => String -> Check r -> Check r
typeBind x = local \Context{..} -> Context
  { typeLevel     = succ typeLevel
  , rawTypeLevels = Map.insert x typeLevel rawTypeLevels
  , rawTypeNames  = x : rawTypeNames
  , typeEnv       = VTVar typeLevel : typeEnv
  , ..
  }

  -- TTVar :: Index -> TType
  -- TTCon :: String -> Unique -> TType
  -- TTArrow :: TType -> TType -> TType
  -- TTForall :: String -> TType -> TType
  -- TTHole :: IORef Hole -> TType

typePrint' :: TType -> Check String
typePrint' t = do
  go False t
  where
    parens True x  = "(" ++ x ++ ")"
    parens False x = x

    go _ (TTVar i) = do
      x <- (!! i) <$> asks rawTypeNames
      pure $ x ++ "@" ++ show (unIndex i)
    go _ (TTCon c _) = pure c
    go p (TTArrow a b) = do
      a <- go True a
      b <- go False b
      pure $ parens p (a ++ " -> " ++ b)
    go p (TTForall x t) = do
      t <- typeBind x $ go False t
      -- VTForall x env t -> do
      --   t <- typeEval (VTVar l : env) t
      --     >>=
      pure $ parens p ("forall " ++ x ++ ". " ++ t)
    go p (TTHole h) = liftIO (readIORef h) >>= \case
      Empty x u l -> pure $ "?" ++ x ++ show (unUnique u) ++ "@" ++ show (unLevel l)
      Full t -> do
        l <- asks typeLevel
        t <- typeQuote l t
        go p t

typePrint :: Dbg => VType -> Check String
typePrint t = do
  l <- asks typeLevel
  go l False t
  where
    parens True x  = "(" ++ x ++ ")"
    parens False x = x

    go l p t = typeForce t >>= \case
      VTVar l' -> do
        x <- (!! lvl2idx l l') <$> asks rawTypeNames
        pure $ x ++ "@" ++ show (unLevel l')
      VTCon c _ -> pure c
      VTArrow a b -> do
        a <- go l True a
        b <- go l False b
        pure $ parens p (a ++ " -> " ++ b)
      VTForall x env t -> do
        t <- typeEval (VTVar l : env) t
          >>= typeBind x . go (succ l) False
        pure $ parens p ("forall " ++ x ++ ". " ++ t)
      VTHole h -> liftIO (readIORef h) >>= \case
        Empty x u l -> pure $ "?" ++ x ++ show (unUnique u) ++ "@" ++ show (unLevel l)
        Full t -> go l p t

typeCheck :: Dbg => RType -> Check TType
typeCheck t = do
  raw <- asks rawTypeLevels
  go (raw, Level $ length raw) t
  where
    go (env, l) (RTVar v) = case Map.lookup v env of
      Just x  -> pure $ TTVar (lvl2idx l x)
      Nothing -> throw $ TypeErrorTypeVariableUnbound v
    go _ (RTCon c) = TTCon c <$> uniqueNew
    go env (RTArrow a b) = TTArrow <$> go env a <*> go env b
    go (env, l) (RTForall x t) = TTForall x
      <$> go (Map.insert x l env, succ l) t

typeCheck' :: Dbg => RType -> Check VType
typeCheck' t = bind2 (asks typeEnv) (typeCheck t) typeEval

exprBind :: Dbg => String -> VType -> Check r -> Check r
exprBind x t = local \ctx -> ctx
  { rawTermTypes = Map.insert x t (rawTermTypes ctx)
  }

exprCheck :: Dbg => Expr -> VType -> Check ()
exprCheck e t = typeForce t >>= \case
  VTForall x env t -> do
    l <- asks typeLevel
    t' <- typeEval (VTVar l : env) t
    typeBind x $ exprCheck e t'
  VTArrow s t | ELambda x Nothing e <- e ->
    exprBind x s $ exprCheck e t
  VTArrow s t | ELambda x (Just s') e <- e -> do
    s' <- typeCheck' s'
    -- NOTE: the order of s s' is due to the contravariace of of function types
    TYPE_UNIFY s s'
    exprBind x s' (exprCheck e t)
  t | ELet x Nothing e1 e2 <- e -> do
    t1 <- exprInfer e1
    exprBind x t1 (exprCheck e2 t)
  t | ELet x (Just t1) e1 e2 <- e -> do
    t1 <- typeCheck' t1
    exprCheck e1 t1
    exprBind x t1 (exprCheck e2 t)
  t -> do
    t' <- exprInfer e
    TYPE_UNIFY t' t

exprInfer :: Dbg => Expr -> Check VType
exprInfer (EVar v) = do
  asks rawTermTypes >>= flip (.) (Map.lookup v) \case
    Nothing -> throw $ TypeErrorVariableUnbound v
    Just t  -> pure t
exprInfer (EAnnot e t) = do
  t <- typeCheck' t
  exprCheck e t $> t
exprInfer (ELambda x (Just t) e) = do
  t <- typeCheck' t
  VTArrow t <$> exprBind x t (exprInfer e)
exprInfer (ELambda x Nothing e) = do
  t <- asks typeLevel >>= typeHole x
  VTArrow t <$> exprBind x t (exprInfer e)
exprInfer (EApply f x) = do
  tf <- exprInfer f
  exprApply f tf x
exprInfer (ELet x (Just t1) e1 e2) = do
  t1 <- typeCheck' t1
  exprCheck e1 t1
  exprBind x t1 (exprInfer e2)
exprInfer (ELet x Nothing e1 e2) = do
  t1 <- exprInfer e1
  exprBind x t1 (exprInfer e2)

exprApply :: Dbg => Expr -> VType -> Expr -> Check VType
exprApply f t e = typeForce t >>= \case
  VTHole h -> liftIO (readIORef h) >>= \case
    Full t -> exprApply f t e
    Empty x _ l -> do
      t1 <- typeHole x l
      t2 <- typeHole x l
      liftIO (writeIORef h (Full (VTArrow t1 t2)))
      exprCheck e t1 $> t2
  VTForall x env t -> do
    h <- asks typeLevel >>= typeHole x
    t <- typeEval (h : env) t
    exprApply f t e
  VTArrow s t      -> do
    exprCheck e s $> t
  _                -> do
    s <- typePrint t
    throw $ TypeErrorNonFunction f s

main :: IO ()
main = runCheck do
  g $ RTForall "x" (RTArrow (RTForall "y" (RTArrow (RTCon "C") (RTArrow (RTVar "x") (RTVar "y")))) (RTVar "x"))
  g $ RTForall "x" (RTForall "x" (RTForall "x" (RTVar "x")))
  g $ RTForall "x" (RTVar "x")
  pure ()
  where
    g x = do
      v <- typeCheck' x
      t <- typeQuote (Level 0) v
      typePrint v >>= liftIO . putStrLn
      typePrint' t >>= liftIO . putStrLn
      liftIO $ putChar '\n'

#if 0
data List a
  = Hole (IORef (Maybe (List a)))
  | Nil
  | Cons String a (List a)
  deriving (Eq)

ofList :: [(String, a)] -> IO (List a)
ofList = pure . foldr (uncurry Cons) Nil

ofList' :: [(String, a)] -> IO (List a)
ofList' = foldr (fmap . uncurry Cons) (Hole <$> newIORef Nothing)

toList :: List a -> IO [Maybe (String, a)]
toList (Hole r) = readIORef r >>= \case
  Nothing -> pure [Nothing]
  Just l -> toList l
toList Nil = pure []
toList (Cons x a xs) = (Just (x,a) :) <$> toList xs

example :: IO ()
example = do
  _x <- ofList [("a", "Int"), ("a", "String"), ("b", "B")]
  _y <- ofList [("b", "String"), ("a", "Int")]
  _z <- ofList [("b", "B"), ("a", "String"), ("a", "Int")]
  -- same x x >>= print
  -- same y y >>= print
  -- same x y >>= print
  -- same y x >>= print
  same _x _z >>= print

{-
<a | r1> ~ <b | r2>
  <a | r3> ~ r2
  r1 ~ <b | r3>

(a :) r1 ~ (b :) r2
  (a :) r3 ~ r2
  r1 ~ (b :) r3
-}

same :: (Show a, Eq a) => List a -> List a -> IO Bool
same x y = go 0 x y
  where
    go n x y = do
      putStr $ replicate (2 * n) ' '
      xS <- printList x
      yS <- printList y
      putStrLn $ xS ++ " ~ " ++ yS
      go' n x y

    go' n (Hole h) t = unifyHole n h t
    go' n t (Hole h) = unifyHole n h t
    go' _ Nil Nil = pure True
    go' n (Cons x t xs) (Cons y s ys) = do
      if x == y then
        if t == s then go (n + 1) xs ys
        else pure False
      else do
        zs <- Hole <$> newIORef Nothing
        a <- go (n + 1) (Cons x t zs) ys
        if a then go (n + 1) (Cons y s zs) xs else pure a
    go' _ _ _ = pure False

    unifyHole n h t = readIORef h >>= \case
      Nothing -> do
        if t == Hole h then pure () else writeIORef h (Just t)
        pure True
      Just t' -> go' n t' t

printList :: Show a => List a -> IO String
printList xs = do
  xs <- fmap (maybe "?" (\(x, t) -> x ++ " :: " ++ show t)) <$> toList xs
  let r = if null xs then "" else foldr1 (\x y -> x <> ", " <> y) xs
  pure $ "[" ++ r ++ "]"
#endif
