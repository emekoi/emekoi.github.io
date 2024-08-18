{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE LambdaCase     #-}

-- TODO: instantiate unknown kinds to KType

module Infer
    ( TypeError (..)
    , TypeErrorKind (..)
    , exprCheck
    , exprInfer
    , exprInferInst
    , exprTopCheck
    , exprTopInfer
    ) where

import Control.Exception          (Exception (..), throwIO)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Foldable              (Foldable (foldl'))
import Data.Functor               (($>))
import Data.Map.Strict            qualified as Map
import Data.Text                  qualified as Text
import Data.Typeable              (Typeable)
import GHC.Stack
import Prelude                    hiding ((!!))
import Syntax

bind2 :: Monad m => m a -> m b -> (a -> b -> m c) -> m c
bind2 x y k = x >>= ((y >>=) . k)

data TypeErrorKind
  = TypeErrorKindMismatch StrictText StrictText
  | TypeErrorKindOccursCheck StrictText StrictText
  | TypeErrorKindNonArrow StrictText
  | TypeErrorTypeMismatch StrictText StrictText
  | TypeErrorTypeOccursCheck StrictText StrictText
  | TypeErrorTypeVariableEscape StrictText
  | TypeErrorTypeVariableUnbound StrictText
  | TypeErrorTypeMissingLabels [StrictText]
  | TypeErrorConstructorUnknown StrictText
  | TypeErrorVariableUnbound StrictText
  | TypeErrorExprNonFunction Expr StrictText
  deriving (Show, Typeable)

data TypeError where TypeError :: Dbg => TypeErrorKind -> TypeError

instance Show TypeError where
  show (TypeError x) = show x ++ "\n" ++ prettyCallStack callStack

instance Exception TypeError where

throw :: (Dbg, MonadIO m) => TypeErrorKind -> m a
throw x = liftIO . throwIO $ withFrozenCallStack (TypeError x)

-- | unify 2 kinds
kindUnify :: Dbg => Kind -> Kind -> Check ()
kindUnify _k1 _k2 = bind2 (kindForce _k1) (kindForce _k2) \cases
  k k' | k == k' -> pure ()
  (KHole h) k2 -> fillHole h k2
  k1 (KHole h) -> fillHole h k1
  (KArrow k1 k2) (KArrow k1' k2') ->
    kindUnify k1 k1' *> kindUnify k2 k2'
  k1 k2 -> do
    s1 <- display k1
    s2 <- display k2
    throw $ TypeErrorKindMismatch s1 s2
  where
    fillHole h k = do
      scopeCheck h k
      writeIORef h (KHFull k)

    scopeCheck _ KType = pure ()
    scopeCheck _ KRow = pure ()
    scopeCheck h (KArrow a b) = do
      kindForce a >>= scopeCheck h
      kindForce b >>= scopeCheck h
    scopeCheck h (KHole h') = do
      when (h == h') do
        s1 <- display _k1
        s2 <- display _k2
        throw (TypeErrorKindOccursCheck s1 s2)
      readIORef h >>= \case
        KHFull t -> scopeCheck h t
        KHEmpty -> pure ()

rowUnify
  :: Dbg => [(Name, VType)] -> [(Name, VType)]
  -> Maybe VType -> Maybe VType -> Check ()
rowUnify ((x, t):xs) ys r r' = do
  (ys, r') <- rowRewrite ys
  rowUnify xs ys r r'
  where
    rowRewrite ((y, t'):ys) | x == y = typeUnify t t' $> (ys, r')
    rowRewrite ((y, t'):ys) = first ((y, t'):) <$> rowRewrite ys
    rowRewrite [] | Just r' <- r' = typeForce r' >>= \case
      VTHole h -> readIORef h >>= \case
        THFull _ -> error "IMPOSSIBLE"
        THEmpty _ _ l _ -> do
          forM_ r $ typeForce >=> \r -> do
            when (r == r') do
              s1 <- display (VTRowExt ((x,t):xs) r)
              s2 <- display r'
              throw (TypeErrorTypeOccursCheck s1 s2)
          r' <- typeHole "r" KRow l
          writeIORef h (THFull $ VTRowExt [(x,t)] r') $> ([], Just r')
      _ -> error "TODO"
    rowRewrite [] = throw (TypeErrorTypeMissingLabels [x])
rowUnify [] [] Nothing r'  = mapM_ (typeUnify (VTRow [])) r'
rowUnify [] ys (Just r) r' = typeUnify r (maybe (VTRow ys) (VTRowExt ys) r')
rowUnify [] ys Nothing _   = throw . TypeErrorTypeMissingLabels $ fst <$> ys

-- | unify 2 types
typeUnify :: Dbg => VType -> VType -> Check ()
typeUnify _t1 _t2 = bind2 (typeForce _t1) (typeForce _t2) \cases
  t t' | t == t' -> pure ()
  (VTHole t1) t2 -> readIORef t1 >>= \case
    THEmpty _ k l _ -> fillHole l k t1 t2
    THFull t1 -> typeUnify t1 t2
  t1 (VTHole t2) -> readIORef t2 >>= \case
    THEmpty _ k l _ -> fillHole l k t2 t1
    THFull t2 -> typeUnify t1 t2
  (VTForall x k e t) (VTForall _ k' e' t') -> do
    kindUnify k k'
    l <- asks typeLevel
    typeBind x k do
      t  <- typeEval (VTVar l k : e) t
      t' <- typeEval (VTVar l k : e') t'
      typeUnify t t'
  (VTVar l k) (VTVar l' k') | l == l' ->
    kindUnify k k'
  (VTCon _ k u) (VTCon _ k' u') | u == u' ->
    kindUnify k k'
  (VTArrow t1 t2) (VTArrow t1' t2') ->
    typeUnify t1 t1' *> typeUnify t2 t2'
  (VTRecord xs) (VTRecord xs') -> do
    typeUnify xs xs'
  (VTRow xs) (VTRow xs') ->
    rowUnify xs xs' Nothing Nothing
  (VTRowExt xs r) (VTRowExt xs' r') ->
    rowUnify xs xs' (Just r) (Just r')
  (VTRow xs) (VTRowExt xs' r') ->
    rowUnify xs xs' Nothing (Just r')
  (VTRowExt xs r) (VTRow xs') ->
    rowUnify xs xs' (Just r) Nothing
  t1 t2 -> do
    s1 <- display t1
    s2 <- display t2
    throw $ TypeErrorTypeMismatch s1 s2
  where
    fillHole l k h t = do
      l' <- asks typeLevel
      k' <- scopeCheck l l' h t
      kindUnify k k'
      writeIORef h (THFull t)

    scopeCheck l l' h (VTForall x k env t) = do
      v <- VTVar <$> asks typeLevel
      typeBind x k $ typeEval (v k : env) t
        >>= scopeCheck l l' h
    scopeCheck l l' _ t@(VTVar x k) = do
      when (l <= x && x < l') do
        s <- display t
        throw (TypeErrorTypeVariableEscape s)
      pure k
    scopeCheck _ _ _ (VTCon _ k _) = pure k
    scopeCheck l l' h (VTArrow a b) = do
      typeForce a >>= scopeCheck l l' h >>= kindUnify KType
      typeForce b >>= scopeCheck l l' h >>= kindUnify KType
      pure KType
    scopeCheck l l' h (VTApply a b) = do
      (k1, k2) <- typeForce a >>= scopeCheck l l' h >>= kindForce >>= \case
        KArrow k1 k2 -> pure (k1, k2)
        KHole h -> do
          k1 <- kindHole
          k2 <- kindHole
          writeIORef h (KHFull (KArrow k1 k2))
          pure (k1, k2)
        _ -> do
          s <- display a
          throw $ TypeErrorKindNonArrow s
      typeForce b >>= scopeCheck l l' h >>= kindUnify k1
      pure k2
    scopeCheck l l' h (VTRow xs) = do
      forM_ xs \(_, t) ->
        scopeCheck l l' h t >>= kindUnify KType
      pure KRow
    scopeCheck l l' h (VTRowExt xs r) = do
      forM_ xs \(_, t) ->
        scopeCheck l l' h t >>= kindUnify KType
      scopeCheck l l' h r >>= kindUnify KRow
      pure KRow
    scopeCheck l l' h (VTRecord xs) = do
      scopeCheck l l' h xs >>= kindUnify KRow
      pure KType
    scopeCheck l l' h (VTHole h') = do
      when (h == h') do
        s1 <- display _t1
        s2 <- display _t2
        throw (TypeErrorTypeOccursCheck s1 s2)
      readIORef h' >>= \case
        THEmpty x k s u -> do
          when (s > l) do
            writeIORef h' (THEmpty x k l u)
          pure k
        THFull t -> scopeCheck l l' h t

-- | compute the kind of a VType
-- typeKind :: VType -> Check Kind
-- typeKind t = typeForce t >>= \case
--   VTVar _ k -> pure k
--   VTCon _ k _ -> pure k
--   VTArrow a b -> do
--     -- TODO: do we need this?
--     typeKind a >>= kindUnify KType
--     typeKind b >>= kindUnify KType
--     pure KType
--   VTApply a b -> do
--     -- TODO: do we need this?
--     typeKind a >>= kindUnify KType
--     typeKind b >>= kindUnify KType
--     pure KType
--   VTForall x k e t -> do
--     v <- VTVar <$> asks typeLevel
--     typeBind x k $ typeEval (v k : e) t
--       >>= typeKind
--   VTHole h -> readIORef h >>= \case
--     THEmpty _ k _ -> pure k
--     THFull t -> typeKind t

-- | convert a RType to VType and compute its kind
typeCheck :: Dbg => RType -> Check (VType, Kind)
typeCheck t = do
  raw <- asks rawTypeLevels
  (t, k) <- go (raw, Level $ length raw) t
  -- TODO: can we build this directly without the later eval step?
  (,k) <$> (asks typeEnv >>= flip typeEval t)
  where
    getKind RKType        = KType
    getKind RKRow         = KRow
    getKind (RKArrow a b) = KArrow (getKind a) (getKind b)

    go (env, l) (RTForall x k t) = do
      k <- maybe kindHole (pure . getKind) k
      (t, k') <- go (Map.insert x (l, k) env, succ l) t
      pure (TTForall x k t, k')
    go (env, l) (RTVar v) = case Map.lookup v env of
      Just (l', k) -> pure (TTVar (lvl2idx l l') k, k)
      Nothing      -> throw $ TypeErrorTypeVariableUnbound v
    go _ (RTCon c) = asks rawTypeCons >>= flip (.) (Map.lookup c) \case
      Nothing -> throw $ TypeErrorConstructorUnknown c
      Just (u, k) -> pure (TTCon c k u, k)
    go acc (RTArrow a b) = do
      (a, k1) <- go acc a
      kindUnify k1 KType
      (b, k2) <- go acc b
      kindUnify k2 KType
      pure (TTArrow a b, KType)
    go acc (RTApply a b) = do
      (a, k1) <- go acc a
      (k1, k2) <- kindForce k1 >>= \case
        KArrow k1 k2 -> pure (k1, k2)
        KHole h -> do
          k1 <- kindHole
          k2 <- kindHole
          writeIORef h (KHFull (KArrow k1 k2))
          pure (k1, k2)
        _ -> do
          s <- display a
          throw $ TypeErrorKindNonArrow s
      (b, k1') <- go acc b
      kindUnify k1 k1'
      pure (TTApply a b, k2)
    go acc (RTRecord xs) = do
      xs <- forM xs \(x, t) -> do
        (t, k) <- go acc t
        kindUnify k KType
        pure (x, t)
      pure (TTRecord (TTRow xs), KType)
    go acc@(env, l) (RTRecordExt xs r) = do
      xs <- forM xs \(x, t) -> do
        (t, k) <- go acc t
        kindUnify k KType
        pure (x, t)
      r <- case Map.lookup r env of
        Nothing      -> throw $ TypeErrorTypeVariableUnbound r
        Just (l', k) -> do
          kindUnify k KRow
          pure $ TTVar (lvl2idx l l') k
      pure (TTRecord (TTRowExt xs r), KType)

-- | convert a RType to VType ensure the kind is Type
typeCheck' :: Dbg => RType -> Check VType
typeCheck' t = do
  (t, k) <- typeCheck t
  kindUnify KType k
  pure t

-- | check an Expr has a given VType
exprCheck :: Dbg => Expr -> VType -> Check ()
exprCheck e t = typeForce t >>= \case
  VTForall x k env t -> do
    l <- asks typeLevel
    t' <- typeEval (VTVar l k : env) t
    typeBind x k $ exprCheck e t'
  VTArrow s t | ELambda x s' e <- e -> do
    forM_ s' (typeCheck' >=> typeUnify s)
    exprBind x s $ exprCheck e t
  t2 | ELet x t1 e1 e2 <- e -> do
    t1 <- case t1 of
      Nothing ->
        exprInfer e1
      Just t1 -> do
        t1 <- typeCheck' t1
        exprCheck e1 t1 $> t1
    exprBind x t1 $ exprCheck e2 t2
  t2 | ELetRec x t1 e1 e2 <- e -> do
    t1 <- maybe (asks typeLevel >>= typeHole x KType) typeCheck' t1
    exprBind x t1 do
      exprCheck e1 t1
      exprCheck e2 t2
  t -> do
    t' <- exprInferInst e
    typeUnify t' t

-- | infer the VType of a given Expr
exprInfer :: Dbg => Expr -> Check VType
exprInfer EUnit = do
  pure (VTCon "Unit" KType (Unique 0))
exprInfer (EInt _) = do
  pure (VTCon "Int" KType (Unique 1))
exprInfer (EVar v) = do
  asks rawTermTypes >>= flip (.) (Map.lookup v) \case
    Nothing -> throw $ TypeErrorVariableUnbound v
    Just t  -> pure t
exprInfer (EAnnot e t) = do
  t <- typeCheck' t
  exprCheck e t $> t
exprInfer (ELambda x t e) = do
  l <- asks typeLevel
  t <- maybe (typeHole x KType l) typeCheck' t
  VTArrow t <$> exprBind x t (exprInferInst e)
exprInfer (EApply f x) = do
  exprInferInst f >>= typeForce >>= apply
  where
    apply (VTHole h) = readIORef h >>= \case
      THEmpty n k l _ -> do
        t1 <- typeHole n k l
        t2 <- typeHole n k l
        writeIORef h (THFull (VTArrow t1 t2))
        exprCheck x t1 $> t2
      THFull t -> apply t
    apply (VTArrow s t) = do
      exprCheck x s $> t
    apply t = do
      s <- display t
      throw $ TypeErrorExprNonFunction f s
exprInfer (ELet x t1 e1 e2) = do
  t1 <- case t1 of
    Nothing ->
      exprInfer e1
    Just t1 -> do
      t1 <- typeCheck' t1
      exprCheck e1 t1 $> t1
  exprBind x t1 (exprInfer e2)
exprInfer (ELetRec x t1 e1 e2) = do
  t1 <- maybe (asks typeLevel >>= typeHole x KType) typeCheck' t1
  exprBind x t1 do
    exprCheck e1 t1
    exprInfer e2
exprInfer (ESelect e l) = do
  t <- asks typeLevel >>= typeHole "t" KType
  r <- asks typeLevel >>= typeHole "r" KRow
  exprCheck e (VTRecord (VTRowExt [(l, t)] r)) $> t
exprInfer (ERestrict e l) = do
  t <- asks typeLevel >>= typeHole "t" KType
  r <- asks typeLevel >>= typeHole "r" KRow
  exprCheck e (VTRecord (VTRowExt [(l, t)] r)) $> VTRecord r
exprInfer (ERecord xs) = do
  xs <- mapM (mapM exprInfer) xs
  pure $ VTRecord (VTRow xs)
exprInfer (ERecordExt xs e) = do
  xs <- mapM (mapM exprInfer) xs
  r <- asks typeLevel >>= typeHole "r" KRow
  exprCheck e (VTRecord r)
  -- TODO: normalize type. we should infer the type of e and depending on
  -- whether it is a concrete record or a record extension, produce a new
  -- concrete record/record extension
  -- exprInfer e >>= typeForce >>= \case
  --   VTRecordExt ys r -> pure ()
  --   VTRecord ys -> pure ()
  pure $ VTRecord (VTRowExt xs r)

-- | infer the VType of a given Expr, instantiating and leading VForalls
exprInferInst :: Dbg => Expr -> Check VType
exprInferInst e = do
  l <- asks typeLevel
  exprInfer e >>= instAll l
  where
    instAll l (VTForall x k env t) = do
      h <- typeHole x k l
      typeEval (h : env) t >>= instAll l
    instAll _ t = pure t

-- | infer and generalize the TType of a given Expr
exprTopInfer :: Dbg => Expr -> Check TType
exprTopInfer e = do
  -- NOTE: uses laziness to compute the number of binders that we will insert at
  -- the start of the final type at the same time as we quote the type
  (t', vs) <- mfix \(~(_, a)) -> do
    t <- exprInfer e
    -- exprCheck e t
    runStateT (go (Level (length a)) t) []

  pure $ foldl' (flip $ uncurry TTForall) t' vs
  where
    go l t = typeForce t >>= \case
      VTForall x k env t -> do
        t' <- typeEval (VTVar l k : env) t
        TTForall x k <$> go (succ l) t'
      VTVar x k -> pure $ TTVar (lvl2idx l x) k
      VTCon c i k -> pure $ TTCon c i k
      VTArrow s t -> TTArrow <$> go l s <*> go l t
      VTApply s t -> TTApply <$> go l s <*> go l t
      VTRow xs -> TTRow <$> mapM (mapM (go l)) xs
      VTRowExt xs r -> TTRowExt <$> mapM (mapM (go l)) xs <*> go l r
      VTRecord xs -> TTRecord <$> go l xs
      VTHole h -> do
        readIORef h >>= \case
          THEmpty n k _ _ -> StateT \vs -> do
            let vl = Level (length vs)
            writeIORef h (THFull (VTVar vl k))
            pure ( TTVar (lvl2idx l vl) k
                 , (n <> Text.pack (show (unLevel vl)), k) : vs
                 )
          THFull t -> go l t

-- | check that an Expr has a given RType and convert RType to a TType
exprTopCheck :: Dbg => Expr -> RType -> Check TType
exprTopCheck e t = do
  t <- typeCheck' t
  exprCheck e t
  asks typeLevel >>= flip typeQuote t
