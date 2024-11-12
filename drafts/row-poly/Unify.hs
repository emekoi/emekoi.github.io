{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module Unify
    ( kindUnify
    , typeUnify
    ) where

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Zip
import Data.Functor               (($>))
import Data.Map.Strict            qualified as Map
import Data.Maybe                 (fromMaybe)
import Data.Set                   qualified as Set
import Error
import Prelude                    hiding ((!!))
import Syntax
import Util

-- | unify 2 kinds
kindUnify :: Dbg => Kind -> Kind -> Check ()
kindUnify _k1 _k2 = bind2 (kindForce _k1) (kindForce _k2) \cases
  k1 k2 | k1 == k2 -> pure ()
  (KHole h) k2 -> fillHole h k2
  k1 (KHole h) -> fillHole h k1
  (KArrow k1s k1) (KArrow k2s k2) | length k1s == length k2s ->
    zipWithM_ kindUnify k1s k2s *> kindUnify k1 k2
  k1 k2 -> do
    s1 <- display k1
    s2 <- display k2
    throw $ TypeErrorKindMismatch s1 s2
  where
    fillHole h k = do
      scopeCheck h k
      writeIORef h (KHFull k)

    scopeCheck h (KHole h') = do
      when (h == h') do
        s1 <- display _k1
        s2 <- display _k2
        throw (TypeErrorKindOccursCheck s1 s2)
      readIORef h >>= \case
        KHFull t -> scopeCheck h t
        KHEmpty -> pure ()
    scopeCheck _ KType = pure ()
    scopeCheck h (KArrow xs t) = do
      mapM_ (kindForce >=> scopeCheck h) xs
      kindForce t >>= scopeCheck h
    scopeCheck _ KRow = pure ()

rowUnify
  :: Dbg => VRow -> VRow
  -> Maybe VType -> Maybe VType -> Check ()
rowUnify xs ys rx ry = do
  forM_ (Map.keysSet xs `Set.intersection` Map.keysSet ys) \n -> do
    when (length xs /= length ys) typeMismatch
    -- zipWithM_ typeUnify (xs Map.! n) (ys Map.! n)
    mapM_ (uncurry typeUnify) $ mzip (xs Map.! n) (ys Map.! n)
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
      typeUnify (VTRowExt ysMissing r) ry'
      typeUnify rx' (VTRowExt xsMissing r)
  where
    toRow r = maybe (VTRow r) (VTRowExt r)
    typeMismatch = do
      s1 <- display (toRow xs rx)
      s2 <- display (toRow ys ry)
      throw (TypeErrorTypeMismatch s1 s2)
    rx' = fromMaybe (VTRow mempty) rx
    ry' = fromMaybe (VTRow mempty) ry
    ysMissing = xs Map.\\ ys
    xsMissing = ys Map.\\ xs

-- | unify 2 types
typeUnify :: Dbg => VType -> VType -> Check ()
typeUnify _t1 _t2 = bind2 (typeForce _t1) (typeForce _t2) \cases
  t1 t2 | t1 == t2 -> pure ()
  (VTHole t1) t2 -> readIORef t1 >>= \case
    THEmpty _ k l _ -> fillHole l k t1 t2
    THFull t1 -> typeUnify t1 t2
  t1 (VTHole t2) -> readIORef t2 >>= \case
    THEmpty _ k l _ -> fillHole l k t2 t1
    THFull t2 -> typeUnify t1 t2
  (VTCon n1 k1) (VTCon n2 k2) | n1 == n2 ->
    kindUnify k1 k2
  (VTVar l1 k1) (VTVar l2 k2) | l1 == l2 ->
    kindUnify k1 k2
  (VTArrow t1s t1) (VTArrow t2s t2) | length t1s == length t2s ->
    zipWithM_ typeUnify t1s t2s *> typeUnify t1 t2
  (VTApply t1 t1s) (VTApply t2 t2s) | length t1s == length t2s ->
    typeUnify t1 t2 *> zipWithM_ typeUnify t1s t2s
  (VTRow fs1) (VTRow fs2) ->
    rowUnify fs1 fs2 Nothing Nothing
  (VTRowExt fs1 r1) (VTRowExt fs2 r2) ->
    rowUnify fs1 fs2 (Just r1) (Just r2)
  (VTRow fs1) (VTRowExt fs2 r2) ->
    rowUnify fs1 fs2 Nothing (Just r2)
  (VTRowExt fs1 r1) (VTRow fs2) ->
    rowUnify fs1 fs2 (Just r1) Nothing
  (VTRecord fs1) (VTRecord fs2) ->
    typeUnify fs1 fs2
  (VTForall x1 k1 e1 t1) (VTForall _ k2 e2 t2) -> do
    kindUnify k1 k2
    l <- asks typeLevel
    typeBind x1 k1 do
      t1 <- typeEval (VTVar l k1 : e1) t1
      t2 <- typeEval (VTVar l k2 : e2) t2
      typeUnify t1 t2
  t1 t2 -> do
    s1 <- display t1
    s2 <- display t2
    throw $ TypeErrorTypeMismatch s1 s2
  where
    fillHole l k h t = do
      baseLevel <- asks typeLevel
      scopeCheck l baseLevel h t >>= kindUnify k
      writeIORef h (THFull t)

    scopeCheck minLevel baseLevel h (VTHole h') = do
      when (h == h') do
        s1 <- display _t1
        s2 <- display _t2
        throw (TypeErrorTypeOccursCheck s1 s2)
      readIORef h' >>= \case
        THEmpty x k l u -> do
          -- NOTE: export h' to the lower level
          when (l > minLevel) do
            writeIORef h' (THEmpty x k minLevel u)
          pure k
        THFull t -> scopeCheck minLevel baseLevel h t
    scopeCheck _ _ _ (VTCon _ k) = pure k
    scopeCheck minLevel baseLevel _ t@(VTVar l k) = do
      -- NOTE: if minLevel <= l, then t was bound by a forall to the right of
      -- the one that bound the hole so solving h to some type containing t
      -- would cause t to escape its scope. however, if l > baseLevel, then t
      -- was bound by a forall in the solution of h, so t does not escape.
      when (minLevel <= l && l < baseLevel) do
        s <- display t
        throw (TypeErrorTypeVariableEscape s)
      pure k
    scopeCheck minLevel baseLevel h (VTArrow ts t) = do
      mapM_ (scopeCheck minLevel baseLevel h >=> kindUnify KType) ts
      scopeCheck minLevel baseLevel h t >>= kindUnify KType
      pure KType
    scopeCheck minLevel baseLevel h (VTApply t ts) =
      scopeCheck minLevel baseLevel h t >>= kindForce >>= \case
        KArrow ks k -> do
          let (lk, lx) = (length ks, length ts)
          void $ zipWithM' ks ts \k t -> do
            scopeCheck minLevel baseLevel h t >>= kindUnify k
          if lk > lx then pure $ KArrow (drop lx ks) k
          else if lk == lx then pure k
          else do
            -- NOTE: due to how the parser is written, we know all arrow kinds
            -- are of the form xs -> Row or xs -> Type, so if length k1 < length
            -- xs, there is no way to unify the kinds
            s <- display (VTApply t (take lk ts))
            throw $ TypeErrorKindNonArrow s
        KHole h' -> do
          ks <- mapM (scopeCheck minLevel baseLevel h) ts
          k <- kindHole
          writeIORef h' (KHFull (KArrow ks k)) $> k
        _ -> do
          s <- display t
          throw $ TypeErrorKindNonArrow s
    scopeCheck minLevel baseLevel h (VTRow fs) = do
      forM_ fs $
        mapM (scopeCheck minLevel baseLevel h >=> kindUnify KType)
      pure KRow
    scopeCheck minLevel baseLevel h (VTRowExt fs r) = do
      forM_ fs $
        mapM (scopeCheck minLevel baseLevel h >=> kindUnify KType)
      scopeCheck minLevel baseLevel h r >>= kindUnify KRow
      pure KRow
    scopeCheck minLevel baseLevel h (VTRecord rs) = do
      scopeCheck minLevel baseLevel h rs >>= kindUnify KRow
      pure KType
    scopeCheck minLevel baseLevel h (VTForall x l env t) = do
      v <- VTVar <$> asks typeLevel
      typeBind x l $ typeEval (v l : env) t
        >>= scopeCheck minLevel baseLevel h
