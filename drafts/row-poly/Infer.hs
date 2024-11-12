{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

-- TODO: instantiate unknown kinds to KType
-- TODO: nail down expected/recieved order in errors
-- TODO: use label maps everywhere

module Infer
    ( TypeError (..)
    , TypeErrorKind (..)
    , exprCheck
    , exprInfer
    , exprInferInst
    , exprTopCheck
    , exprTopInfer
    ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Foldable              (Foldable (foldl'))
import Data.Functor               (($>))
import Data.Map.Strict            qualified as Map
import Data.Text                  qualified as Text
import Error
import Prelude                    hiding ((!!))
import Syntax
import Unify

zipWithM' :: Applicative m => [a] -> [b] -> (a -> b -> m c) -> m [c]
zipWithM' xs ys f = zipWithM f xs ys

-- | convert a RType to VType ensure the kind is Type
typeCheck :: Dbg => RType -> Check VType
typeCheck t = do
  raw <- asks rawTypeLevels
  env <- asks typeEnv
  (vt, _, k) <- go (Level $ length raw, raw, env) t
  kindUnify k KType $> vt
  where
    getKind RKType         = KType
    getKind RKRow          = KRow
    getKind (RKArrow xs t) = KArrow (map getKind xs) (getKind t)

    go (l, ctx, env) (RTForall x rk rt) = do
      k <- maybe kindHole (pure . getKind) rk
      (_, tt, k') <- go (succ l, Map.insert x (l, k) ctx, VTVar l k : env) rt
      pure (VTForall x k env tt, TTForall x k tt, k')
    go (l, ctx, _) (RTVar v) = case Map.lookup v ctx of
      Just (l', k) -> pure (VTVar l' k, TTVar (lvl2idx l l') k, k)
      Nothing      -> throw $ TypeErrorTypeVariableUnbound v
    go _ (RTCon c) = asks rawTypeCons >>= flip (.) (Map.lookup c) \case
      Nothing -> throw $ TypeErrorConstructorUnknown c
      Just k -> pure (VTCon c k, TTCon c k, k)
    go acc (RTArrow rt1s rt2) = do
      (vt1s, tt1s) <- unzip <$> forM rt1s \rt -> do
        (vt, tt, k) <- go acc rt
        kindUnify k KType
        pure (vt, tt)
      (vt2, tt2, k) <- go acc rt2
      kindUnify k KType
      pure (VTArrow vt1s vt2, TTArrow tt1s tt2, KType)
    go acc (RTApply rt1 rt2s) = do
      (vt1, tt1, k) <- go acc rt1
      kindForce k >>= \case
        KArrow k1s k2 -> do
          let (lk, lrt) = (length k1s, length rt2s)
          (vt2s, tt2s) <- unzip <$> zipWithM' k1s rt2s \k t -> do
            (vt2, tt2, k') <- go acc t
            kindUnify k k' $> (vt2, tt2)
          if lk > lrt then
            pure (VTApply vt1 vt2s, TTApply tt1 tt2s, KArrow (drop lrt k1s) k2)
          else if lk < lrt then do
            -- NOTE: due to how the parser is written, we know all arrow kinds are
            -- of the form xs -> Row or xs -> Type, so if length k1 < length xs,
            -- there is no way to unify the kinds
            -- (vt3, tt3, k3) <- unzip3 <$> mapM (go acc) (drop lk rt2s)
            -- k4 <- kindHole
            -- kindUnify k2 (KArrow k3 k4)
            -- pure ( VTApply (VTApply vt1 vt2s) vt3
            --      , TTApply (TTApply tt1 tt2s) tt3
            --      , KArrow (k1s ++ k3) k4
            --      )
            s <- display (TTApply tt1 (take lk tt2s))
            throw $ TypeErrorKindNonArrow s
          else
            pure (VTApply vt1 vt2s, TTApply tt1 tt2s, k2)
        KHole h' -> do
          (vt2s, tt2s, k1s) <- unzip3 <$> mapM (go acc) rt2s
          k2 <- kindHole
          writeIORef h' (KHFull (KArrow k1s k2)) $>
            (VTApply vt1 vt2s, TTApply tt1 tt2s, k2)
        _ -> do
          s <- display tt1
          throw $ TypeErrorKindNonArrow s
    go acc (RTRecord rrs) = do
      (ls, vts, tts) <- unzip3 <$> forM rrs \(rl, rt) -> do
        (vt, tt, k) <- go acc rt
        kindUnify k KType
        pure (rl, vt, tt)
      pure (VTRecord (VTRow (zip ls vts)), TTRecord (TTRow (zip ls tts)), KType)
    go acc@(l, ctx, _) (RTRecordExt rrs rt) = do
      (ls, vts, tts) <- unzip3 <$> forM rrs \(rl, rt) -> do
        (vt, tt, k) <- go acc rt
        kindUnify k KType
        pure (rl, vt, tt)
      case Map.lookup rt ctx of
        Nothing      -> throw $ TypeErrorTypeVariableUnbound rt
        Just (l', k) -> do
          kindUnify k KRow
          let (vt, tt) = (VTVar l' KRow, TTVar (lvl2idx l l') KRow)
          pure ( VTRecord (VTRowExt (zip ls vts) vt)
               , TTRecord (TTRowExt (zip ls tts) tt), KType)

-- | check an Expr has a given VType
exprCheck :: Dbg => Expr -> VType -> Check ()
exprCheck e t = typeForce t >>= \case
  VTForall x k env t -> do
    l <- asks typeLevel
    t' <- typeEval (VTVar l k : env) t
    typeBind x k $ exprCheck e t'
  VTArrow ss t | ELambda xs e <- e -> do
    let (ls, lx) = (length ss, length xs)
    xs' <- zipWithM' ss xs \s (x, s') -> do
      forM_ s' (typeCheck >=> typeUnify s)
      pure (x, s)
    if ls > lx then
      -- curried application
      foldr (uncurry exprBind) (exprCheck e (VTArrow (drop lx ss) t)) xs'
    else if ls < lx then do
      -- t must be another function type
      foldr (uncurry exprBind) (exprCheck (ELambda (drop ls xs) e) t) xs'
    else
      -- fully saturated application
      foldr (uncurry exprBind) (exprCheck e t) xs'
  t2 | ELet x t1 e1 e2 <- e -> do
    t1 <- case t1 of
      Nothing ->
        exprInfer e1
      Just t1 -> do
        t1 <- typeCheck t1
        exprCheck e1 t1 $> t1
    exprBind x t1 $ exprCheck e2 t2
  t2 | ELetRec x t1 e1 e2 <- e -> do
    t1 <- maybe (asks typeLevel >>= typeHole x KType) typeCheck t1
    exprBind x t1 do
      exprCheck e1 t1
      exprCheck e2 t2
  t -> do
    t' <- exprInferInst e
    typeUnify t' t

-- | infer the VType of a given Expr
exprInfer :: Dbg => Expr -> Check VType
exprInfer EUnit = do
  pure (VTCon "Unit" KType)
exprInfer (EInt _) = do
  pure (VTCon "Int" KType)
exprInfer (EVar v) = do
  asks rawTermTypes >>= flip (.) (Map.lookup v) \case
    Nothing -> throw $ TypeErrorVariableUnbound v
    Just t  -> pure t
exprInfer (EAnnot e t) = do
  t <- typeCheck t
  exprCheck e t $> t
exprInfer (ELambda xs e) = do
  l <- asks typeLevel
  ts <- forM xs \(x, t) ->
   (x,) <$> maybe (typeHole x KType l) typeCheck t
  -- foldr/foldl doesn't matter since bindings are unique and non-dependent
  -- r <- foldl' (flip $ uncurry exprBind) (exprInferInst e) ts
  r <- foldr (uncurry exprBind) (exprInferInst e) ts
  pure $ VTArrow (snd <$> ts) r
exprInfer (EApply f xs) =
  exprInferInst f >>= typeForce >>= \case
    VTArrow ts r -> apply ts r [] xs
    t -> apply [] t [] xs
  where
    -- NOTE: we traverse the arrow type alongside the spine, collected applied
    -- arguments until we hit a mismatch or the end of the spine. if we hit the
    -- end, the return type is whats left, if there was a mismatch we try to
    -- resolve it. if we can't we report that the application of f to the spine
    -- so far doesn't yield a function we can apply to the rest of the spine.
    apply (t:ts) r xs (y:ys) = exprCheck y t *> apply ts r (y : xs) ys
    apply [] r _ []         = pure r
    apply ts r _ []         = pure $ VTArrow ts r
    apply [] r xs ys        = typeForce r >>= \case
      VTArrow ts r -> apply ts r xs ys
      VTHole h -> readIORef h >>= \case
        THFull _ -> error "IMPOSSIBLE"
        THEmpty n k l _ -> do
          -- NOTE: the paper has us generate holes to check the spine against,
          -- but doing so is functionally the same as just inferring the type of
          -- the spine and instantiating it so thats what we do instead
          ts <- mapM exprInferInst ys
          r <- typeHole n k l
          writeIORef h (THFull (VTArrow ts r)) $> r
      _ -> throw $ TypeErrorExprNonFunction (Text.pack . show $ EApply f (reverse xs))
exprInfer (ELet x t1 e1 e2) = do
  t1 <- case t1 of
    Nothing ->
      exprInfer e1
    Just t1 -> do
      t1 <- typeCheck t1
      exprCheck e1 t1 $> t1
  exprBind x t1 (exprInfer e2)
exprInfer (ELetRec x t1 e1 e2) = do
  t1 <- maybe (asks typeLevel >>= typeHole x KType) typeCheck t1
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
  pure $ VTRecord (VTRowExt xs r)

-- | infer the VType of a given Expr, instantiating and leading VForalls
exprInferInst :: Dbg => Expr -> Check VType
exprInferInst e = do
  l <- asks typeLevel
  exprInfer e >>= typeForce >>= instAll l
  where
    instAll l (VTForall x k env t) = do
      h <- typeHole x k l
      typeEval (h : env) t >>= instAll l
    instAll _ t = pure t

-- | infer and generalize the TType of a given Expr
exprTopInfer :: Dbg => Expr -> Check TType
exprTopInfer e = do
  t <- exprInfer e
  exprCheck e t
  -- NOTE: uses laziness to compute the number of binders that we will insert at
  -- the start of the final type at the same time as we quote the type
  (t', vs) <- mfix \(~(_, a)) -> do
    runStateT (go (Level (length a)) t) []

  pure $ foldl' (flip $ uncurry TTForall) t' vs
  where
    go l t = typeForce t >>= \case
      VTForall x k env t -> do
        t' <- typeEval (VTVar l k : env) t
        TTForall x k <$> go (succ l) t'
      VTVar x k -> pure $ TTVar (lvl2idx l x) k
      VTCon c i -> pure $ TTCon c i
      VTArrow t1s t2 -> TTArrow <$> mapM (go l) t1s <*> go l t2
      VTApply t1 t2s -> TTApply <$> go l t1 <*> mapM (go l) t2s
      VTRow rs -> TTRow <$> mapM (mapM (go l)) rs
      VTRowExt rs r -> TTRowExt <$> mapM (mapM (go l)) rs <*> go l r
      VTRecord r -> TTRecord <$> go l r
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
  t <- typeCheck t
  exprCheck e t
  asks typeLevel >>= flip typeQuote t
