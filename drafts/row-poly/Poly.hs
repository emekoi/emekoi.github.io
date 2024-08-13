{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE FieldSelectors  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Poly
    ( main
    ) where

import Control.Exception          (Exception (..), handle, throwIO)
import Control.Monad
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor               (($>))
import Data.IntMap                qualified as IntMap
import Data.IORef                 (IORef, atomicModifyIORef', newIORef,
                                   readIORef, writeIORef)
import Data.List                  qualified as List
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Foldable
import Data.Typeable              (Typeable)
import GHC.Stack
import Prelude                    hiding ((!!))

type Dbg = HasCallStack

bind2 :: Monad m => m a -> m b -> (a -> b -> m c) -> m c
bind2 x y k = x >>= ((y >>=) . k)

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
  EUnit :: Expr
  EVar :: String -> Expr
  EApply :: Expr -> Expr -> Expr
  ELambda :: String -> Maybe RType -> Expr -> Expr
  ELet :: String -> Maybe RType -> Expr -> Expr -> Expr
  EAnnot :: Expr -> RType -> Expr
  deriving (Eq, Show)

data TypeErrorKind
  = TypeErrorMismatch String String
  | TypeErrorOccursCheck String String
  | TypeErrorTypeVariableEscape String
  | TypeErrorTypeVariableUnbound String
  | TypeErrorVariableUnbound String
  | TypeErrorNonFunction Expr String
  deriving (Show, Typeable)

data TypeError where TypeError :: Dbg => TypeErrorKind -> TypeError

instance Show TypeError where
  show (TypeError x) = show x ++ "\n" ++ prettyCallStack callStack

instance Exception TypeError where

throw :: (Dbg, MonadIO m) => TypeErrorKind -> m a
throw x = liftIO . throwIO $ withFrozenCallStack (TypeError x)

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

typeForce :: (Dbg, MonadIO m) => VType -> m VType
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

_typeQuote' :: (Dbg, MonadIO m) => Level -> Level -> VType -> m TType
_typeQuote' b l t = typeForce t >>= \case
  VTVar x -> pure $ TTVar (lvl2idx l x)
  VTCon c i -> pure $ TTCon c i
  VTArrow s t -> TTArrow
    <$> _typeQuote' b l s
    <*> _typeQuote' b l t
  VTForall x env t -> do
    t' <- typeEval (VTVar l : env) t
    TTForall x <$> _typeQuote' b (succ l) t'
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

typeUnify :: Dbg => VType -> VType -> Check ()
typeUnify _t1 _t2 = bind2 (typeForce _t1) (typeForce _t2) \cases
  t t' | t == t' -> pure ()
  (VTHole t1) t2 -> liftIO (readIORef t1) >>= \case
    Empty _ _ l -> fillHole l t1 t2
    Full t1 -> typeUnify t1 t2
  t1 (VTHole t2) -> liftIO (readIORef t2) >>= \case
    Empty _ _ l -> fillHole l t2 t1
    Full t2 -> typeUnify t1 t2
  (VTVar l) (VTVar l') | l == l' -> pure ()
  (VTCon _ u) (VTCon _ u') | u == u' -> pure ()
  (VTArrow t1 t2) (VTArrow t1' t2') ->
    typeUnify t1 t1' *> typeUnify t2 t2'
  (VTForall x e t) (VTForall _ e' t') -> do
    l <- asks typeLevel
    typeBind x do
      t  <- typeEval (VTVar l : e) t
      t' <- typeEval (VTVar l : e') t'
      typeUnify t t'
  t1 t2 -> do
    s1 <- typePrint t1
    s2 <- typePrint t2
    throw $ TypeErrorMismatch s1 s2
  where
    fillHole :: Dbg => Level -> IORef Hole -> VType -> Check ()
    fillHole l h t = do
      l' <- asks typeLevel
      scopeCheck l l' h t
      liftIO $ writeIORef h (Full t)

    scopeCheck :: Dbg => Level -> Level -> IORef Hole -> VType -> Check ()
    scopeCheck l l' _ t@(VTVar x) =
      when (l <= x && x < l') do
        s <- typePrint t
        throw (TypeErrorTypeVariableEscape s)
    scopeCheck _ _ _ VTCon{} = pure ()
    scopeCheck l l' h (VTArrow a b) = do
      typeForce a >>= scopeCheck l l' h
      typeForce b >>= scopeCheck l l' h
    scopeCheck l l' h (VTForall x env t) = do
      v <- VTVar <$> asks typeLevel
      typeBind x $ typeEval (v : env) t
        >>= scopeCheck l l' h
    scopeCheck l l' h (VTHole h') = do
      when (h == h') do
        s1 <- typePrint _t1
        s2 <- typePrint _t2
        throw (TypeErrorOccursCheck s1 s2)
      liftIO (readIORef h') >>= \case
        Empty x u s -> when (s > l) do
          liftIO $ writeIORef h' (Empty x u l)
        Full t -> scopeCheck l l' h t

typeBind :: Dbg => String -> Check r -> Check r
typeBind x = local \Context{..} -> Context
  { typeLevel     = succ typeLevel
  , rawTypeLevels = Map.insert x typeLevel rawTypeLevels
  , rawTypeNames  = x : rawTypeNames
  , typeEnv       = VTVar typeLevel : typeEnv
  , ..
  }

typePrint' :: Dbg => TType -> Check String
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
      pure $ parens p ("forall " ++ x ++ ". " ++ t)
    go p (TTHole h) = liftIO (readIORef h) >>= \case
      Empty x u l -> pure $ "?" ++ x ++ "." ++ show (unUnique u) ++ "@" ++ show (unLevel l)
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
        Empty x u l -> pure $ "?" ++ x ++ "." ++ show (unUnique u) ++ "@" ++ show (unLevel l)
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
    typeUnify s s'
    exprBind x s' (exprCheck e t)
  t | ELet x Nothing e1 e2 <- e -> do
    t1 <- exprInfer e1
    exprBind x t1 (exprCheck e2 t)
  t | ELet x (Just t1) e1 e2 <- e -> do
    t1 <- typeCheck' t1
    exprCheck e1 t1
    exprBind x t1 (exprCheck e2 t)
  t -> do
    t' <- exprInferInst e
    typeUnify t' t

exprInfer :: Dbg => Expr -> Check VType
exprInfer EUnit = do
  pure (VTCon "UNIT" (Unique (-1)))
exprInfer (EVar v) = do
  asks rawTermTypes >>= flip (.) (Map.lookup v) \case
    Nothing -> throw $ TypeErrorVariableUnbound v
    Just t  -> pure t
exprInfer (EAnnot e t) = do
  t <- typeCheck' t
  exprCheck e t $> t
exprInfer (ELambda x t e) = do
  l <- asks typeLevel
  t <- maybe (typeHole x l) typeCheck' t
  -- t <- VTArrow t <$> exprBind x t (exprInferInst e)
  VTArrow t <$> exprBind x t (exprInfer e)
exprInfer (EApply f x) = do
  exprInferInst f >>= typeForce >>= apply
  where
    apply (VTHole h) = liftIO (readIORef h) >>= \case
      Empty n _ l -> do
        t1 <- typeHole n l
        t2 <- typeHole n l
        liftIO (writeIORef h (Full (VTArrow t1 t2)))
        exprCheck x t1 $> t2
      Full t -> apply t
    apply (VTArrow s t) = do
      exprCheck x s $> t
    apply t = do
      s <- typePrint t
      throw $ TypeErrorNonFunction f s
exprInfer (ELet x Nothing e1 e2) = do
  t1 <- exprInfer e1
  exprBind x t1 (exprInfer e2)
exprInfer (ELet x (Just t1) e1 e2) = do
  t1 <- typeCheck' t1
  exprCheck e1 t1
  exprBind x t1 (exprInfer e2)

exprInferInst :: Dbg => Expr -> Check VType
exprInferInst e = do
  l <- asks typeLevel
  exprInfer e >>= instAll l
  where
    instAll l (VTForall x env t) = do
      h <- typeHole x l
      typeEval (h : env) t >>= instAll l
    instAll _ t = pure t

-- _exprInferGen :: Dbg => Expr -> Check VType
-- _exprInferGen e = do
--   l <- asks typeLevel
--   t <- exprInfer e
--   xs <- gen l (succ l) t []
--   liftIO $ print xs
--   pure t
--   where
--     succBy n 0 = n
--     succBy n m = succBy (succ n) (m - 1)

--     -- NOTE: since we do not know how many foralls will be inserted, we need to
--     -- make 2 passes over the type to count the variables to generralize and to do the
--     -- gen :: (Dbg, MonadIO m) => Level -> Level -> VType -> [(String,Unique)] -> m [(String,Unique)]
--     gen b l t vs = typeForce t >>= \case
--       VTArrow s t -> gen b l s vs >>= gen b l t
--       VTForall _ env t -> do
--         t' <- typeEval (VTVar l : env) t
--         gen b (succ l) t' vs
--       VTHole h ->
--         liftIO (readIORef h) >>= \case
--           Empty n x l | l >= b -> do
--             let vs' = (n,x) : vs
--             liftIO $ writeIORef h (Full (VTVar (succBy b (length vs))))
--             pure vs'
--           Full t -> gen b l t vs
--           Empty n x l -> do
--             liftIO $ print (n, x, l)
--             pure vs
--       _ -> pure vs

_exprInferGen :: Dbg => Expr -> Check TType
_exprInferGen e = do
  l <- asks typeLevel
  -- t <- local (\Context{..} -> Context {typeLevel = succ typeLevel,..}) $
  --   exprInfer e
  t <- exprInfer e
  -- (_, xs) <- gen l (succ l) t (l, mempty)
  (_, (l', xs)) <- runStateT (go l l t) (l, [])
  z <- foldl' (\t (n, x) -> TTForall n t) <$> typeQuote l' t <*> pure xs
  -- liftIO $ print xs
  -- t' <- typeQuote (succBy l (length xs)) t
  -- foldM `flip` t' `flip` xs $ \t (h, x, l) -> do
  --   liftIO $ writeIORef h (Full (VTVar l))
  --   pure
  -- forAccumM (l, t) (xs )

  pure z
  where
    succBy n 0 = n
    succBy n m = succBy (succ n) (m - 1)

    go b l t = typeForce t >>= \case
      VTArrow s t -> go b l s *> go b l t
      VTForall _ env t -> typeEval (VTVar l : env) t >>= go b (succ l)
      VTHole h -> do
        liftIO (readIORef h) >>= \case
          Empty n x l | l >= b -> StateT \(vl, vs) -> do
            liftIO $ writeIORef h (Full (VTVar vl))
            pure ((), (succ vl, (n, x) : vs))
          _ -> pure ()
      _ -> pure ()

    -- NOTE: since we do not know how many foralls will be inserted, we need to
    -- make 2 passes

    gen b l t acc@(vl, vm) = typeForce t >>= \case
      VTArrow s t -> gen b l s acc >>= gen b l t
      VTForall _ env t -> do
        t' <- typeEval (VTVar l : env) t
        gen b (succ l) t' acc
      VTHole h ->
        liftIO (readIORef h) >>= \case
          Empty n x l | l > b, Nothing <- IntMap.lookup (unUnique x) vm ->
            pure (succ vl, IntMap.insert (unUnique x) (h, n, vl) vm)
          _ -> pure acc
      _ -> pure acc

    -- go b l t = typeForce t >>= \case
    --   VTVar x -> pure $ TTVar (lvl2idx l x)
    --   VTCon c i -> pure $ TTCon c i
    --   VTArrow s t -> TTArrow
    --     <$> go b l s
    --     <*> go b l t
    --   VTForall x env t -> do
    --     t' <- typeEval (VTVar l : env) t
    --     TTForall x <$> go b (succ l) t'
    --   VTHole h -> do
    --     liftIO (readIORef h) >>= \case
    --       Empty n x l | l > b -> StateT \(vl, vs) -> do
    --         liftIO $ writeIORef h (Full (VTVar vl))
    --         pure ((), (succ vl, (n,x) : vs))
    --       _ -> pure ()
    --     pure $ TTHole h

main :: IO ()
main = runCheck do
  let idT x = RTForall x (RTArrow (RTVar x) (RTVar x))

  go $ ELambda "y" (Just (idT "a")) (ELet "x" (Just (idT "b")) (ELambda "z" Nothing (EApply (EVar "y") (EVar "z"))) (EApply (EVar "x") EUnit))
  go $ ELambda "y" (Just (idT "a")) (ELet "x" (Just (idT "b")) (EVar "y") (EApply (EVar "x") EUnit))

  go $ ELambda "y" Nothing (ELet "x" Nothing (ELambda "z" Nothing (EApply (EVar "y") (EVar "z"))) (EApply (EVar "x") EUnit))
  go $ ELambda "y" Nothing (ELet "x" Nothing (EVar "y") (EApply (EVar "x") EUnit))

  go $ ELambda "y" (Just (idT "a")) (ELet "x" Nothing (ELambda "z" Nothing (EApply (EVar "y") (EVar "z"))) (EApply (EVar "x") EUnit))
  go $ ELambda "y" (Just (idT "a")) (ELet "x" Nothing (EVar "y") (EApply (EVar "x") EUnit))

  go $ ELambda "y" Nothing (ELet "x" (Just (idT "a")) (ELambda "z" Nothing (EApply (EVar "y") (EVar "z"))) (EApply (EVar "x") EUnit))
  go $ ELambda "y" Nothing (ELet "x" (Just (idT "a")) (EVar "y") (EApply (EVar "x") EUnit))
  go $ foldl' (\x n -> ELambda n Nothing x) (EVar "x") ["x", "y", "z", "w"]

  pure ()
  where
    go x = ReaderT \r -> handle @TypeError (\x -> print x *> putChar '\n') $ flip runReaderT r do
      t  <- _exprInferGen x
      -- t' <- typeQuote (Level 0) t
      -- typePrint t >>= liftIO . putStrLn
      typePrint' t >>= liftIO . putStrLn
      liftIO $ putChar '\n'

  --   g x = do
  --     v <- typeCheck' x
  --     t <- typeQuote (Level 0) v
  --     typePrint v >>= liftIO . putStrLn
  --     typePrint' t >>= liftIO . putStrLn
  --     liftIO $ putChar '\n'

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
