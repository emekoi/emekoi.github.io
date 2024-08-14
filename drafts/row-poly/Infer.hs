{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE LambdaCase     #-}

-- module Infer
--     ( Expr (..)
--     , RType (..)
--     , TType (..)
--     , TypeError (..)
--     , TypeErrorKind (..)
--     , VType (..)
--     , exprCheck
--     , exprInfer
--     , exprInferInst
--     , exprTopCheck
--     , exprTopInfer
--     , runCheck
--     , typeCheck
--     , typeCheck'
--     , typeEval
--     , typePrint
--     , typePrint'
--     , typeQuote
--     ) where

module Infer
    ( TypeError (..)
    , TypeErrorKind (..)
    , exprCheck
    , exprInfer
    , exprInferInst
    , exprTopCheck
    , exprTopInfer
    , runCheck
    , typePrint
    , typePrint'
    ) where

import Control.Exception          (Exception (..), throwIO)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Foldable
import Data.Functor               (($>))
import Data.IORef                 (IORef)
import Data.Map.Strict            qualified as Map
import Data.Text                  qualified as Text
import Data.Typeable              (Typeable)
import GHC.Stack
import Prelude                    hiding ((!!))
import Syntax

bind2 :: Monad m => m a -> m b -> (a -> b -> m c) -> m c
bind2 x y k = x >>= ((y >>=) . k)

data TypeErrorKind
  = TypeErrorMismatch StrictText StrictText
  | TypeErrorOccursCheck StrictText StrictText
  | TypeErrorTypeVariableEscape StrictText
  | TypeErrorTypeVariableUnbound StrictText
  | TypeErrorVariableUnbound StrictText
  | TypeErrorNonFunction Expr StrictText
  deriving (Show, Typeable)

data TypeError where TypeError :: Dbg => TypeErrorKind -> TypeError

instance Show TypeError where
  show (TypeError x) = show x ++ "\n" ++ prettyCallStack callStack

instance Exception TypeError where

throw :: (Dbg, MonadIO m) => TypeErrorKind -> m a
throw x = liftIO . throwIO $ withFrozenCallStack (TypeError x)

typeUnify :: Dbg => VType -> VType -> Check ()
typeUnify _t1 _t2 = bind2 (typeForce _t1) (typeForce _t2) \cases
  t t' | t == t' -> pure ()
  (VTHole t1) t2 -> holeRead t1 >>= \case
    Empty _ _ l -> fillHole l t1 t2
    Full t1 -> typeUnify t1 t2
  t1 (VTHole t2) -> holeRead t2 >>= \case
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
      holeWrite h (Full t)

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
      holeRead h >>= \case
        Empty x u s -> when (s > l) do
          holeWrite h' (Empty x u l)
        Full t -> scopeCheck l l' h t

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
  t <- maybe (holeNew x l) typeCheck' t
  -- t <- VTArrow t <$> exprBind x t (exprInferInst e)
  VTArrow t <$> exprBind x t (exprInfer e)
exprInfer (EApply f x) = do
  exprInferInst f >>= typeForce >>= apply
  where
    apply (VTHole h) = holeRead h >>= \case
      Empty n _ l -> do
        t1 <- holeNew n l
        t2 <- holeNew n l
        holeWrite h (Full (VTArrow t1 t2))
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
      h <- holeNew x l
      typeEval (h : env) t >>= instAll l
    instAll _ t = pure t

exprTopInfer :: Dbg => Expr -> Check TType
exprTopInfer e = do
  -- NOTE: uses laziness to compute the number of binders that we will insert at
  -- the start of the final type at the same time as we quote the type
  (t', vs) <- mfix \(~(_, a)) -> do
    t <- exprInfer e
    runStateT (go (Level (length a)) t) []

  pure $ foldl' (flip TTForall) t' vs
  where
    go l t = typeForce t >>= \case
      VTVar x -> pure $ TTVar (lvl2idx l x)
      VTCon c i -> pure $ TTCon c i
      VTArrow s t -> TTArrow
        <$> go l s
        <*> go l t
      VTForall x env t -> do
        t' <- typeEval (VTVar l : env) t
        TTForall x <$> go (succ l) t'
      VTHole h -> do
        holeRead h >>= \case
          Empty n (Unique x) _ -> StateT \vs -> do
            let vl = Level (length vs)
            holeWrite h (Full (VTVar vl))
            pure (TTVar (lvl2idx l vl), (n <> Text.pack (show x)) : vs)
          Full t -> go l t

exprTopCheck :: Dbg => Expr -> RType -> Check TType
exprTopCheck e t = do
  t <- typeCheck t
  v <- asks typeEnv >>= flip typeEval t
  exprCheck e v
  pure t

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
