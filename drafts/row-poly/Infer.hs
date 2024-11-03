{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP            #-}
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

import Control.Exception          (Exception (..), throwIO)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Foldable              (Foldable (foldl'))
import Data.Functor               (($>))
import Data.Map.Strict            qualified as Map
import Data.Maybe                 (fromMaybe)
import Data.Set                   qualified as Set
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
  | TypeErrorExprNonFunction Expr
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
  (KArrow k1 k2) (KArrow k1' k2') | length k1 == length k1' ->
    mapM_ (uncurry kindUnify) (zip k1 k1') *> kindUnify k2 k2'
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
    scopeCheck h (KArrow xs t) = do
      mapM_ (kindForce >=> scopeCheck h) xs
      kindForce t >>= scopeCheck h
    scopeCheck h (KHole h') = do
      when (h == h') do
        s1 <- display _k1
        s2 <- display _k2
        throw (TypeErrorKindOccursCheck s1 s2)
      readIORef h >>= \case
        KHFull t -> scopeCheck h t
        KHEmpty -> pure ()

rowToMap :: [(Name, VType)] -> Map.Map Name [VType]
rowToMap xs = foldr `flip` Map.empty `flip` xs $ \(x, t) r ->
  -- NOTE: right associated due to foldr
  Map.insertWith (++) x [t] r

mapToRow :: Map.Map Name [VType] -> [(Name, VType)]
mapToRow = concatMap (\(x, ts) -> (x,) <$> ts) . Map.toList

rowUnify
  :: Dbg => [(Name, VType)] -> [(Name, VType)]
  -> Maybe VType -> Maybe VType -> Check ()
rowUnify (rowToMap -> xs) (rowToMap -> ys) rx ry = do
  forM_ (Map.keysSet xs `Set.intersection` Map.keysSet ys) \n -> do
    when (length xs /= length ys) typeMismatch
    mapM_ (uncurry typeUnify) $ zip (xs Map.! n) (ys Map.! n)
  case (null ysMissing, null xsMissing) of
    -- they share the same fields so unify rx ry
    (True, True)  -> typeUnify rx' ry'
    -- ys has fields not in xs, so unify rx (ys - xs | ry)
    (True, False) -> typeUnify rx' (toRow xsMissing ry)
    -- xs has fields not in ys, so unify (xs - ys | rx) ry
    (False, True) -> typeUnify (toRow ysMissing rx) ry'
    -- if rx == ry at this point, unification is not possible.
    -- NOTE: this equivalent to the side condition given in the paper since
    -- rewriting produces either empty or singleton substitutions since
    -- variables can only occur in the tail position of a row.
    -- NOTE: could be either a rigid mismatch or an occurs check issue but
    -- calling it a rigid mismatch makes for clearer errors
    (False, False) | rx == ry -> typeMismatch
    -- otherwise, unify rx (ys - xs | r) and unify (xs - ys | r) ry
    (False, False) -> do
      r <- asks typeLevel >>= typeHole "r" KRow
      typeUnify rx' (VTRowExt (mapToRow xsMissing) r)
      typeUnify (VTRowExt (mapToRow ysMissing) r) ry'
  where
    toRow (mapToRow -> r) = maybe (VTRow r) (VTRowExt r)
    typeMismatch = do
      s1 <- display (toRow xs rx)
      s2 <- display (toRow ys ry)
      throw (TypeErrorTypeMismatch s1 s2)
    rx' = fromMaybe (VTRow []) rx
    ry' = fromMaybe (VTRow []) ry
    ysMissing = xs Map.\\ ys
    xsMissing = ys Map.\\ xs

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
  (VTArrow t1 t2) (VTArrow t1' t2') | length t1 == length t1' ->
    mapM_ (uncurry typeUnify) (zip t1 t1') *> typeUnify t2 t2'
  (VTApply t1 t2) (VTApply t1' t2') | length t2 == length t2' ->
    typeUnify t1 t1' *> mapM_ (uncurry typeUnify) (zip t2 t2')
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
      -- NOTE: if l <= x, then x was bound by a forall to the right of the one
      -- that bound the hole so solving h to some type containing t would cause
      -- t to escape its scope. however, if x > l', then t was bound by a forall
      -- in the solution of h, so t does not escape.
      when (l <= x && x < l') do
        s <- display t
        throw (TypeErrorTypeVariableEscape s)
      pure k
    scopeCheck _ _ _ (VTCon _ k _) = pure k
    scopeCheck l l' h (VTArrow as b) = do
      mapM_ (scopeCheck l l' h >=> kindUnify KType) as
      scopeCheck l l' h b >>= kindUnify KType
      pure KType
    scopeCheck l l' h (VTApply f xs) =
      scopeCheck l l' h f >>= kindForce >>= \case
        KArrow k1 k2 -> do
          let (lk, lx) = (length k1, length xs)
          forM_ (zip k1 xs) \(k, t) -> do
            scopeCheck l l' h t >>= kindUnify k
          if lk > lx then pure $ KArrow (drop lx k1) k2
          else if lk == lx then pure k2
          else do
            -- NOTE: due to how the parser is written, we know all arrow kinds are
            -- of the form xs -> Row or xs -> Type, so if length k1 < length xs,
            -- there is no way to unify the kinds
            s <- display (VTApply f (take lk xs))
            throw $ TypeErrorKindNonArrow s
        KHole h' -> do
          k1 <- mapM (scopeCheck l l' h) xs
          k2 <- kindHole
          writeIORef h' (KHFull (KArrow k1 k2)) $> k2
        _ -> do
          s <- display f
          throw $ TypeErrorKindNonArrow s
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
    getKind RKType         = KType
    getKind RKRow          = KRow
    getKind (RKArrow xs t) = KArrow (map getKind xs) (getKind t)

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
    go acc (RTArrow as b) = do
      as <- forM as \a -> do
        (a, k1) <- go acc a
        kindUnify k1 KType
        pure a
      (b, k2) <- go acc b
      kindUnify k2 KType
      pure (TTArrow as b, KType)
    go acc (RTApply f xs) = do
      (f, k1) <- go acc f
      kindForce k1 >>= \case
        KArrow k1 k2 -> do
          let (lk, lx) = (length k1, length xs)
          xs' <- forM (zip k1 xs) \(k, t) -> do
            (t, k') <- go acc t
            kindUnify k k' $> t
          if lk > lx then pure (TTApply f xs', KArrow (drop lx k1) k2)
          else if lk == lx then pure (TTApply f xs', k2)
          else do
            -- NOTE: due to how the parser is written, we know all arrow kinds are
            -- of the form xs -> Row or xs -> Type, so if length k1 < length xs,
            -- there is no way to unify the kinds
            s <- display (TTApply f (take lk xs'))
            throw $ TypeErrorKindNonArrow s
        KHole h' -> do
          k1 <- mapM (go acc) xs
          k2 <- kindHole
          writeIORef h' (KHFull (KArrow (snd <$> k1) k2)) $>
            (TTApply f (fst <$> k1), k2)
        _ -> do
          s <- display f
          throw $ TypeErrorKindNonArrow s
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
  VTArrow ss t | ELambda xs e <- e -> do
    let (ls, lx) = (length ss, length xs)
    xs' <- forM (zip ss xs) \(s, (x, s')) -> do
      forM_ s' (typeCheck' >=> typeUnify s)
      pure (x, s)
    if ls > lx then
      -- curried application
      foldr (uncurry exprBind) (exprCheck e (VTArrow (drop lx ss) t)) xs'
    else if ls < lx then do
      -- t must be another function type
      foldr (uncurry exprBind) (exprCheck (ELambda (drop ls xs) e) t) xs'
    else foldr (uncurry exprBind) (exprCheck e t) xs'
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
exprInfer (ELambda xs e) = do
  l <- asks typeLevel
  ts <- forM xs \(x, t) ->
   (x,) <$> maybe (typeHole x KType l) typeCheck' t
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
    -- so far doesn't yield a function we apply to the rest of the spine.
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

          -- -- NOTE: its fine to duplicate names and levels
          -- ts <- replicateM (length ys) (typeHole n k l)
          -- r <- typeHole n k l
          -- writeIORef h (THFull (VTArrow ts r))
          -- mapM_ (uncurry exprCheck) (zip ys ts) $> r

          ts <- mapM exprInferInst ys
          r <- typeHole n k l
          writeIORef h (THFull (VTArrow ts r)) $> r
      _ -> throw $ TypeErrorExprNonFunction (EApply f (reverse xs))
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
    exprCheck e t
    runStateT (go (Level (length a)) t) []

  pure $ foldl' (flip $ uncurry TTForall) t' vs
  where
    go l t = typeForce t >>= \case
      VTForall x k env t -> do
        t' <- typeEval (VTVar l k : env) t
        TTForall x k <$> go (succ l) t'
      VTVar x k -> pure $ TTVar (lvl2idx l x) k
      VTCon c i k -> pure $ TTCon c i k
      VTArrow ss t -> TTArrow <$> mapM (go l) ss <*> go l t
      VTApply f xs -> TTApply <$> go l f <*> mapM (go l) xs
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
