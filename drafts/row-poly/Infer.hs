{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}

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
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor               (($>))
import Data.List.NonEmpty         (NonEmpty)
import Data.Map.Strict            qualified as Map
import Data.Text                  qualified as Text
import Error
import Prelude                    hiding ((!!))
import Syntax
import Unify
import Util

listToRow :: [(Name, r)] -> Map.Map Name (NonEmpty r)
listToRow fs = foldr `flip` Map.empty `flip` fs $ \(x, t) r ->
  -- NOTE: right associated due to foldr
  Map.insertWith (<>) x [t] r

listToRow' :: [Name] -> [r] -> Map.Map Name (NonEmpty r)
listToRow' xs ts = listToRow $ zip xs ts

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

    go _ (RTCon c) = asks typeCons >>= flip (.) (Map.lookup c) \case
      Nothing -> throw $ TypeErrorTypeConstructorUnknown c
      Just k -> pure (VTCon c k, TTCon c k, k)
    go (l, ctx, _) (RTVar v) = case Map.lookup v ctx of
      Just (l', k) -> pure (VTVar l' k, TTVar (lvl2idx l l') k, k)
      Nothing      -> throw $ TypeErrorTypeVariableUnbound v
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
      pure ( VTRecord (VTRow (listToRow' ls vts))
           , TTRecord (TTRow (listToRow' ls tts))
           , KType
           )
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
          pure ( VTRecord (VTRowExt (listToRow' ls vts) vt)
               , TTRecord (TTRowExt (listToRow' ls tts) tt)
               , KType
               )
    go acc@(_, _, env) (RTForall rxs rt) = do
      xs <- forM rxs \(x, rk) -> (x,) <$> maybe kindHole (pure . getKind) rk
      acc' <- foldM `flip` acc `flip` xs $ \(l, ctx, env) (x, k) -> do
        pure (succ l, Map.insert x (l, k) ctx, VTVar l k : env)
      (_, tt, k') <- go acc' rt
      pure (VTForall xs env tt, TTForall xs tt, k')

-- | check an Expr has a given VType
exprCheck :: Dbg => RExpr -> VType -> Check ()
exprCheck e t = typeForce t >>= \case
  VTForall xs env t -> do
    l <- asks typeLevel
    typeBindAll xs $ typeEval (snd $ typeEnvExtend l env xs) t
      >>= exprCheck e
  VTArrow ss t | RELambda xs e <- e -> do
    let (ls, lx) = (length ss, length xs)
    xs' <- zipWithM' ss xs \s (x, s') -> do
      forM_ s' (typeCheck >=> typeUnify s)
      pure (x, s)
    if ls > lx then
      -- curried application
      foldr (uncurry exprBind) (exprCheck e (VTArrow (drop lx ss) t)) xs'
    else if ls < lx then do
      -- t must be another function type
      foldr (uncurry exprBind) (exprCheck (RELambda (drop ls xs) e) t) xs'
    else
      -- fully saturated application
      foldr (uncurry exprBind) (exprCheck e t) xs'
  t2 | RELet x t1 e1 e2 <- e -> do
    t1 <- case t1 of
      Nothing ->
        exprInfer e1
      Just t1 -> do
        t1 <- typeCheck t1
        exprCheck e1 t1 $> t1
    exprBind x t1 $ exprCheck e2 t2
  t2 | RELetRec x t1 e1 e2 <- e -> do
    t1 <- maybe (asks typeLevel >>= typeHole x KType) typeCheck t1
    exprBind x t1 do
      exprCheck e1 t1
      exprCheck e2 t2
  t -> do
    t' <- exprInferInst e
    typeUnify t' t

-- | infer the VType of a given Expr
exprInfer :: Dbg => RExpr -> Check VType
exprInfer (REInt _) = do
  pure (VTCon "Int" KType)
exprInfer (REVar v) = do
  asks rawTermTypes >>= flip (.) (Map.lookup v) \case
    Nothing -> throw $ TypeErrorTermVariableUnbound v
    Just t  -> pure t
exprInfer (RECon c) = do
  asks rawTermCons >>= flip (.) (Map.lookup c) \case
    Nothing -> throw $ TypeErrorTermConstructorUnknown c
    Just t  -> pure t
exprInfer (RELambda xs e) = do
  l <- asks typeLevel
  ts <- forM xs \(x, t) ->
   (x,) <$> maybe (typeHole x KType l) typeCheck t
  -- NOTE: using exprInferInst in on e results in a eager system, using
  -- exprInfer results in a lazy system (cf. "Seeking Stability by Being Lazy
  -- and Shallow")
  -- NOTE: foldr since we may make xs a telescope later on
  r <- foldr (uncurry exprBind) (exprInferInst e) ts
  pure $ VTArrow (snd <$> ts) r
exprInfer (RELet x t1 e1 e2) = do
  t1 <- case t1 of
    Nothing ->
      exprInfer e1
    Just t1 -> do
      t1 <- typeCheck t1
      exprCheck e1 t1 $> t1
  exprBind x t1 (exprInfer e2)
exprInfer (RELetRec x t1 e1 e2) = do
  t1 <- maybe (asks typeLevel >>= typeHole x KType) typeCheck t1
  exprBind x t1 do
    exprCheck e1 t1
    exprInfer e2
exprInfer (REApply f xs) =
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
    apply [] r _ []          = pure r
    apply ts r _ []          = pure $ VTArrow ts r
    apply [] r xs ys         = typeForce r >>= \case
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
      _ -> throw . TypeErrorTermNonFunction $
        Text.pack . show $ REApply f (reverse xs)
exprInfer (RESelect e l) = do
  t <- asks typeLevel >>= typeHole "t" KType
  r <- asks typeLevel >>= typeHole "r" KRow
  exprCheck e (VTRecord (VTRowExt [(l, [t])] r)) $> t
exprInfer (RERestrict e l) = do
  t <- asks typeLevel >>= typeHole "t" KType
  r <- asks typeLevel >>= typeHole "r" KRow
  exprCheck e (VTRecord (VTRowExt [(l, [t])] r)) $> VTRecord r
exprInfer (RERecord xs) = do
  xs <- mapM (mapM exprInfer) xs
  pure $ VTRecord (VTRow (listToRow xs))
exprInfer (RERecordExt xs e) = do
  xs <- mapM (mapM exprInfer) xs
  r <- asks typeLevel >>= typeHole "r" KRow
  exprCheck e (VTRecord r)
  pure $ VTRecord (VTRowExt (listToRow xs) r)
exprInfer (REAnnot e t) =do
  t <- typeCheck t
  exprCheck e t $> t

-- | infer the VType of a given Expr, instantiating and leading VForalls
exprInferInst :: Dbg => RExpr -> Check VType
exprInferInst e = do
  l <- asks typeLevel
  exprInfer e >>= typeForce >>= instAll l
  where
    instAll l (VTForall xs env t) = do
      (l', env') <- foldM `flip` (l, env) `flip` xs $ \(l, env) (x, k) -> do
        h <- typeHole x k l
        pure (succ l, h : env)
      typeEval env' t >>= instAll l'
    instAll _ t = pure t

-- | infer and generalize the TType of a given Expr
exprTopInfer :: Dbg => RExpr -> Check TType
exprTopInfer e = do
  t <- exprInfer e
  exprCheck e t
  -- NOTE: uses laziness to compute the number of binders that we will insert at
  -- the start of the final type at the same time as we quote the type
  (t', vs) <- mfix \(~(_, a)) -> do
    runStateT (go (Level (length a)) t) []

  pure $ TTForall (reverse vs) t'
  where
    go l t = typeForce t >>= \case
      VTHole h -> do
        readIORef h >>= \case
          THEmpty n k _ _ -> StateT \vs -> do
            let vl = Level (length vs)
            writeIORef h (THFull (VTVar vl k))
            pure ( TTVar (lvl2idx l vl) k
                 , (n <> Text.pack (show (unLevel vl)), k) : vs
                 )
          THFull t -> go l t
      VTCon c i -> pure $ TTCon c i
      VTVar x k -> kindForce k >>= \case
        k@KHole{} -> lift (kindUnify k KType) $> TTVar (lvl2idx l x) KType
        k -> pure $ TTVar (lvl2idx l x) k
      VTArrow t1s t2 -> TTArrow <$> mapM (go l) t1s <*> go l t2
      VTApply t1 t2s -> TTApply <$> go l t1 <*> mapM (go l) t2s
      VTRow rs -> TTRow <$> mapM (mapM (go l)) rs
      VTRowExt rs r -> TTRowExt <$> mapM (mapM (go l)) rs <*> go l r
      VTRecord r -> TTRecord <$> go l r
      VTForall xs env t -> do
        let (l', env') = typeEnvExtend l env xs
        t' <- typeEval env' t
        TTForall xs <$> go l' t'

-- | check that an Expr has a given RType and convert RType to a TType
exprTopCheck :: Dbg => RExpr -> RType -> Check TType
exprTopCheck e t = do
  t <- typeCheck t
  exprCheck e t
  asks typeLevel >>= flip typeQuote t
