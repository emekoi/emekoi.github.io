{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-unused-imports   #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedRecordDot        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Elab
    ( Elab
    , elaborate
    , runElab
    ) where

-- TODO: the continuations we generate for pattern matching have messed up
-- binding levels. we need to refactor how we generate contiuations
-- TODO: switch to using de bruijn indices
-- TODO: we need to fix the generation of bindings and continuations for pattern
-- matching.

import Control.Applicative
import Control.Category           qualified ((>>>))
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Zip
import Core                       as C
import Data.Bifunctor
import Data.Bool                  (bool)
import Data.Default.Class
import Data.Foldable
import Data.Functor
import Data.Functor.Identity
import Data.Graph                 qualified as Graph
import Data.IntMap.Strict         (IntMap)
import Data.IntMap.Strict         qualified as IntMap
import Data.IntSet                (IntSet)
import Data.IntSet                qualified as IntSet
import Data.List                  qualified as List
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Maybe                 (isJust)
import Data.Sequence              (Seq (..))
import Data.Sequence              qualified as Seq
import Data.Set                   (Set)
import Data.Set                   qualified as Set
import Data.Text                  (Text)
import Data.Traversable
import Error
import GHC.Exts                   (coerce)
import GHC.Stack
import Lens.Micro
import Lens.Micro.GHC             ()
import Parser                     qualified as P
import Prettyprinter              qualified as Pretty
import Util
import Witherable
import Unsafe.Coerce

data UsageInfo r = Usage
  { usageInfo :: IntMap (r, IntSet)
  , termIds   :: Map Text Int
  , fresh     :: Int
  }
  deriving (Show)

instance Default (UsageInfo r) where
  def = Usage mempty mempty 0

usageId :: (MonadState (UsageInfo r) m) => Text -> m Int
usageId x = state \u@(Usage {..}) ->
  case Map.lookup x termIds of
    Just i -> (i, u)
    Nothing ->
      ( fresh
      , u
        { fresh = fresh + 1
        , termIds = Map.insert x fresh termIds}
      )

usageDepend :: (MonadState (UsageInfo r) m) => Int -> r -> IntSet -> m ()
usageDepend x r d = modify' \Usage{..} ->
  Usage { usageInfo = IntMap.insert x (r, d) usageInfo, .. }

usageDepend' :: (Semigroup r, MonadState (UsageInfo r) m) => Int -> r -> IntSet -> m ()
usageDepend' x r d = modify' \Usage{..} ->
  Usage { usageInfo = IntMap.insertWith (<>) x (r, d) usageInfo, .. }

usageSCCs :: (MonadState (UsageInfo r) m) => m [Graph.SCC r]
usageSCCs = gets \(Usage {..}) ->
  Graph.stronglyConnComp $ IntMap.foldrWithKey go [] usageInfo
  where go x (k, xs) r = (k, x, IntSet.toList xs) : r

data ElabState = ElabState
  { fileName    :: FilePath
  , fresh       :: IORef Int
  , types       :: IORef (Map Text TypeId)
  , typeInfo    :: IORef (IntMap TypeInfo)
  , dataCons    :: IORef (Map Text DataId)
  , dataConInfo :: IORef (IntMap DataInfo)
  , primitives  :: IORef (Map Text Type)
  , globalTerms :: IORef (Map Text Var)
  , localTerms  :: Map Text Var
  , termLevel   :: Int
  }

newtype Elab s
  = Elab (ReaderT ElabState IO s)
  deriving
    ( Alternative
    , Applicative
    , Functor
    , Monad
    , MonadFail
    , MonadFix
    , MonadIO
    , MonadPlus
    , MonadReader ElabState
    )

throwElab :: ErrMsg -> [(Range, Marker ErrMsg)] -> Elab a
throwElab msg xs = Elab $ ReaderT \s -> throwIO . Error $
  [Err' msg (first (P.r2p s.fileName) <$> xs)]

errQuote :: (Pretty.Pretty p) => p -> ErrMsg
errQuote = ErrPretty Pretty.squotes

runElab :: FilePath -> Elab s -> IO s
runElab fp (Elab s) = do
  ctx <- ElabState fp
    <$> newIORef 0
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef mempty
    <*> newIORef mempty
    <*> pure mempty
    <*> pure 0
  runReaderT s ctx

fresh :: Elab Int
fresh = ask >>= \s -> do
  fresh <- readIORef s.fresh
  modifyIORef s.fresh succ
  pure fresh

freshName :: (Text -> Int -> x) -> Text -> Elab x
freshName k x = fresh <&> k x

freshMeta :: Elab Type
freshMeta = do
  id <- fresh
  meta <- Trivial <$> newIORef Nothing
  pure $ TyMeta id meta

freshLocal :: Text -> Type -> Elab Var
freshLocal x t = freshName (\x i -> Var (Local x i) t) x

freshLabel :: Text -> Seq Type -> Elab Label
freshLabel x args = freshName (\x i -> Label (Local x i) args) x

freshGlobal :: Text -> Type -> Elab Var
freshGlobal x t = freshName (\x i -> Var (Global x i) t) x

tyErrorExpect
  :: (HasRange r, Pretty.Pretty a2, Pretty.Pretty a3)
  => ErrMsg -> a2 -> a3 -> r -> Elab a1
tyErrorExpect thing expect got r =
  throwElab [ "expected", thing, "of type", errQuote expect ]
    [ (range r, This [
        thing, "has type", errQuote got
      ])
    ]

(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>>) = (Control.Category.>>>)

findTypeId :: P.Name Range -> Elab TypeId
findTypeId (P.Name r n) = do
  ElabState{types} <- ask
  readIORef types >>= Map.lookup n >>> \case
    Just id -> pure id
    Nothing -> throwElab
      [ "unkown type", errQuote n ]
      [ (r, This "used here") ]

findTypeInfo :: HasCallStack => TypeId -> Elab TypeInfo
findTypeInfo (TypeId t) = do
  ElabState{typeInfo} <- ask
  readIORef typeInfo >>= IntMap.lookup t.id >>> \case
    Just id -> pure id
    Nothing -> error
      "compiler error: assigned type id to non-existent type"

makeTypeId :: Bool -> P.Name a -> Elab TypeId
makeTypeId magic (P.Name _ n) = do
  ElabState{types} <- ask
  readIORef types >>= Map.lookup n >>> \case
    Just id -> pure id
    Nothing -> do
      id <- TypeId <$> freshName f n
      modifyIORef types (Map.insert n id)
      pure id
  where
    f x i | magic = Global x (-i - 1)
    f x i = Global x i

findDataId :: P.Name Range -> Elab DataId
findDataId (P.Name r n) = do
  ElabState{dataCons} <- ask
  readIORef dataCons >>= Map.lookup n >>> \case
    Just id -> pure id
    Nothing -> throwElab
      [ "unknown constructor", errQuote n ]
      [ (r, This "used here") ]

findDataInfo :: HasCallStack => DataId -> Elab DataInfo
findDataInfo (DataId d) = do
  ElabState{dataConInfo} <- ask
  readIORef dataConInfo >>= IntMap.lookup d.id >>> \case
    Just i -> pure i
    Nothing -> error
      "compier error: assigned constructor id to non-existent constructor"

makeDataId :: P.Name Range -> Elab DataId
makeDataId (P.Name r c) = do
  ElabState{dataCons} <- ask
  readIORef dataCons >>= Map.lookup c >>> \case
    Nothing -> do
      id <- DataId <$> freshName Global c
      modifyIORef dataCons (Map.insert c id)
      pure id
    Just id -> do
      info <- findDataInfo id
      throwElab
        [ "redefinition of constructor", errQuote c ]
        [ (info.range, This "first defined here"), (r, This "redefined here") ]

tyCheckType :: HasCallStack => P.Type Range -> Elab Type
tyCheckType (P.TyData _ c)       = TyData <$> findTypeId c
tyCheckType (P.TyFun _ args ret) = TyFun
    <$> traverse tyCheckType args
    <*> tyCheckType ret

tyCheckDataDecl :: HasCallStack => Bool -> P.Name Range -> Seq (P.DataCon Range) -> Elab ()
tyCheckDataDecl magic t cs = do
  TypeId tid <- findTypeId t
  ElabState{typeInfo,dataConInfo} <- ask
  readIORef typeInfo >>= IntMap.lookup tid.id >>> \case
    Just info -> do
      throwElab
        [ "redefinition of", if tid.id < 0 then "builtin" else "", "type", errQuote t ]
        [ (info.range, This "first defined here")
        , (range t, This "redefined here")
        ]
    Nothing -> modifyIORef typeInfo . IntMap.insert tid.id $
      TypeInfo { name = tid, range = range t, span = bool (Just $ length cs) Nothing magic }
  forM_ cs \(P.DataCon _ name xs) -> do
    DataId did <- makeDataId name
    fields <- mapM tyCheckType xs
    modifyIORef dataConInfo . IntMap.insert did.id $
      DataInfo { name = did, range = range t, typeOf = TypeId tid, .. }

tyMagic :: Text -> Elab Type
tyMagic n = TyData <$> makeTypeId True (P.Name () n)

tyInt :: Elab Type
tyInt = tyMagic "Int"

tyString :: Elab Type
tyString = tyMagic "String"

lookupVar :: P.Name Range -> Elab Var
lookupVar (P.Name r v) = do
  ElabState{localTerms, globalTerms} <- ask
  case Map.lookup v localTerms of
    Just v  -> pure v
    Nothing ->
      readIORef globalTerms >>= Map.lookup v >>> \case
        Just v -> pure v
        Nothing ->
          throwElab "variable not in scope"
          [(r, This $ errPretty v)]

-- bindVar :: Type -> Var
-- bindVar = coerce . bindVars @Identity . coerce

-- bindVar :: Type -> (Var -> Elab r) -> Elab r
-- bindVar t k = bindVars @Identity (coerce t) (k . coerce)

-- bindVars :: (Traversable t) => t Type -> t Var
-- bindVars ts = flip evalState 0 $
--   mapM (\t -> state \i -> (Var (Bound "x" i) t, i + 1)) ts

-- bindVars :: (Traversable t) => t Type -> (t Var -> Elab r) -> Elab r
-- bindVars ts k = do
--   ElabState{termIndex} <- ask
--   let
--     (vs, termIndex') = flip runState termIndex $
--       mapM (\t -> state \i -> (Var (Bound "x" i) t, i + 1)) ts
--   local (\ctx -> ctx
--     { termIndex = termIndex'
--     }) (k vs)

bindVar :: forall a r. (Maybe (P.Name a), Type) -> (Var -> Elab r) -> Elab r
bindVar t k = bindVars @Identity @a (coerce t) (k . coerce)

bindVars :: (Traversable t) => t (Maybe (P.Name a), Type) -> (t Var -> Elab r) -> Elab r
bindVars ts k = do
  ElabState{termLevel,localTerms} <- ask
  let ((level, localTerms'), vs) = mapAccumL (f termLevel) (0, localTerms) ts
  local (\ctx -> ctx
    { termLevel = ctx.termLevel + level
    , localTerms = localTerms'
    }) (k vs)
  where
    g m v x = maybe m (\(P.Name _ x) -> Map.insert x v m) x
    f l (i, m) (x, t) =
      ( (i + 1, g m (Var (Bound "x" $ l + i) t) x)
      , Var (Bound "x" i) t
      )

-- bindVars :: (Traversable t) => t Type -> (t Var -> Elab r) -> Elab r
-- bindVars ts k =
--   let
--     vs = flip evalState 0 $
--       mapM (\t -> state \i -> (Var (Bound "x" i) t, i + 1)) ts
--   in local (\ctx -> ctx
--     { termIndex = ctx.termIndex + length ts
--     }) (k vs)

data Level
  = Low
  | High

data NESeq x = NESeq x (Seq x)
  deriving (Eq, Foldable, Ord, Show)

fromSeq :: HasCallStack => Seq a -> NESeq a
fromSeq Empty      = error "empty Seq"
fromSeq (x :<| xs) = NESeq x xs

data Pattern l where
  PWild :: Type -> Pattern 'High
  PAs :: Text -> Maybe (Pattern 'Low) -> Pattern 'High
  PInt :: Integer -> Pattern l
  PString :: Text -> Pattern l
  PData :: DataId -> Seq (Pattern 'High) -> Pattern l
  POr :: NESeq (Pattern 'Low) -> Maybe (Pattern 'High) -> Pattern 'High

deriving instance Show (Pattern l)
deriving instance Eq (Pattern l)
deriving instance Ord (Pattern l)

lowerPat :: Pattern l -> Maybe (Pattern 'Low)
lowerPat (PInt i)     = Just $ PInt i
lowerPat (PString i)  = Just $ PString i
lowerPat (PData c ps) = Just $ PData c ps
lowerPat _            = Nothing

raisePat :: Pattern l -> Pattern 'High
raisePat p = unsafeCoerce p

patternTypeInfo :: Pattern 'Low -> Elab TypeInfo
patternTypeInfo (PInt _) = makeTypeId True (P.Name () "Int")
  >>= findTypeInfo
patternTypeInfo (PString _) = makeTypeId True (P.Name () "String")
  >>= findTypeInfo
patternTypeInfo (PData did _) = do
  dinfo <- findDataInfo did
  findTypeInfo dinfo.typeOf

data MatchRow = Row
  { cols  :: Map Var (Pattern 'Low)
  , guard :: Maybe (P.Expr Range, Pattern 'Low)
  -- , vars  :: Map Text Var
  --  TODO: make this a term to lower in
  , body  :: Elab Term
  }

rColsL :: Lens' MatchRow (Map Var (Pattern 'Low))
rColsL f x = (\x' -> x {cols = x'}) <$> f x.cols

-- rVarsL :: Lens' MatchRow (Map Text Var)
-- rVarsL f x = (\x' -> x {vars = x'}) <$> f x.vars

rowMatchVar :: Var -> Pattern 'Low -> MatchRow -> MatchRow
rowMatchVar x p = rColsL %~ Map.insert x p

-- rowBindVar :: Text -> Var -> MatchRow -> MatchRow
-- rowBindVar x v = rVarsL %~ Map.insert x v

newtype PatternMatrix
  = Matrix { rows :: Seq MatchRow }

handlePattern
  :: Var
  -> Pattern l
  -> MatchRow
  -> (MatchRow -> b)
  -> b
handlePattern var pat row f =
    f $ case lowerPat pat of
      Nothing   -> row & rColsL %~ Map.delete var
      Just pat' -> rowMatchVar var pat' row

rowDefault :: Var -> PatternMatrix -> PatternMatrix
rowDefault var mat = mat { rows = foldl' f Empty mat.rows }
  where
    f acc row = case Map.lookup var row.cols of
      -- add the row as is since its a wildcard
      Nothing                               -> acc :|> row
      -- delete every other row
      Just _                                -> acc

rowSpecialize :: Var -> AltCon -> PatternMatrix -> PatternMatrix
rowSpecialize var alt mat = mat { rows = foldl' (f alt) Empty mat.rows }
  where
    f alt acc row =
      case Map.lookup var row.cols of
        Nothing -> acc :|> row
        Just p -> g alt row acc p

    -- delete the column since there are no sub patterns
    g (VInt i) row acc (PInt i') | i == i' =
      acc :|> (row & rColsL %~ Map.delete var)
    -- delete the column since there are no sub patterns
    g (VString s) row acc (PString s') | s == s' =
      acc :|> (row & rColsL %~ Map.delete var)
    -- otherwise replace the column with its sub patterns
    g (VData con vars) row acc (PData con' ps) | con == con' =
      let g row i p = handlePattern (vars `Seq.index` i) p row id in
        (acc :|>) $ Seq.foldlWithIndex g(row & rColsL %~ Map.delete var) ps
    -- since the constrcutors don't match delete the row
    g _ _ acc _ = acc

-- NOTE: partial Map.!, foldl1
columnPick :: PatternMatrix -> Elab (Var, TypeInfo)
columnPick (Matrix Empty) = error "empty match matrix"
columnPick (Matrix rows@(row :<| _)) =
  let (k, _) = List.foldl1 findMax . Map.toList $
        foldl' mkMap (row.cols $> (0 :: Int)) rows in
  fmap (k,) . patternTypeInfo $ row.cols Map.! k
  where
    findMax a b
      | snd b > snd a = b
      | otherwise     = a
    mkMap acc r =
      Map.foldlWithKey' (\acc k _ -> Map.insertWith (+) k 1 acc) acc r.cols

matchComplete :: TypeInfo -> Set a -> Bool
matchComplete (TypeInfo{span}) cons = Just (length cons) == span

mapFoldlM :: (acc -> k -> v -> Elab acc) -> acc -> Map k v -> Elab acc
mapFoldlM f z0 xs = Map.foldrWithKey c return xs z0
  where c x y k z = f z x y >>= k; {-# INLINE c #-}

-- matchCompile :: PatternMatrix -> Elab Term
-- matchCompile (Matrix Empty) = do
--   t <- tyString
--   x <- freshLocal "s" t
--   pure $ TLetV x (VString "in-exhaustive match") (TError x)
-- matchCompile (Matrix (Row col Nothing body :<| _))
--   | null col = pure body
-- -- matchCompile k (Matrix (row@(Row col (Just (v, p)) _ _) :<| rs) pick)
-- --   | null col =
-- --     let row' = rowMatchVar v p row { guard = Nothing } in
-- --       matchCompile k (Matrix (row' :<| rs) pick)
-- matchCompile mat@(Matrix rows) = do
--   -- pick a variable to scrutinize
--   (var, tinfo) <- columnPick mat
--   -- collect the heads of constructors tested against var
--   -- then compile the subtrees and combine them into a switch
--   (def, cons, cases) <- foldM (goRow var) (Nothing, mempty, Empty) rows
--   case def of
--     Just {} ->
--       pure $ TMatch var cases def
--     Nothing | matchComplete tinfo cons ->
--       pure $ TMatch var cases Nothing
--     Nothing ->
--       TMatch var cases . Just
--         <$> defaultCase var
--   where
--     defaultCase v = matchCompile (rowDefault v mat)

--     -- filter out repeated test against constructors
--     specialize v acc@(d, cs, ms) c = do
--       if Set.notMember c cs then do
--         let body = rowSpecialize v c mat
--         m <- Alt c <$> matchCompile body
--         pure (d, Set.insert c cs, ms :|> m)
--       else pure acc

--     -- for each constructor, specialize the pattern matrix
--     goHead _ acc@(Just {}, _, _) _ = pure acc
--     goHead v acc (PInt i)          = specialize v acc (VInt i)
--     goHead v acc (PString s)       = specialize v acc (VString s)
--     goHead v acc (PData c vs)      = do
--       ElabState{localTerms} <- ask
--       cinfo <- findDataInfo c
--       ((_, localTerms'), vs) <- mapAccumM f
--         (length vs - 1, localTerms)
--         (mzip vs cinfo.fields)
--       local (\ctx -> ctx
--         { termLevel = ctx.termLevel + length vs
--         , localTerms = localTerms'
--         }) $ specialize v acc (VData c vs)
--       where
--         f (i, m :: Map Text Var) (PWild (Right (x, p)), t) = do
--           liftIO $ print m
--           f (i,  Map.insert x (Var (Bound "x" i) t) m) (raisePat p, t)
--         f (i, m) (_, t) = pure ((i - 1, m), Var (Bound "x" i) t)

--     goRow _  acc@(Just {}, _, _) _ = pure acc
--     goRow v acc r                  = maybe (pure acc) (goHead v acc) (Map.lookup v r.cols)

matchCompile :: PatternMatrix -> Elab Term
matchCompile (Matrix Empty) = do
  t <- tyString
  x <- freshLocal "s" t
  pure $ TLetV x (VString "in-exhaustive match") (TError x)
matchCompile (Matrix (Row col Nothing body :<| _))
  | null col = body
matchCompile mat@(Matrix rows) = mdo
  -- pick a variable to scrutinize
  (var, tinfo) <- columnPick mat
  -- collect the heads of constructors tested against var
  -- then compile the subtrees and combine them into a switch
  (def, cons, cases, match) <- foldM (goRow var) (Nothing, mempty, Empty, body) rows
  body <- case def of
    Just {} ->
      pure $ TMatch var cases def
    Nothing | matchComplete tinfo cons ->
      pure $ TMatch var cases Nothing
    Nothing ->
      TMatch var cases . Just
        <$> defaultCase var
  pure match
  where
    defaultCase v = matchCompile (rowDefault v mat)

    -- patternBinds (VInt _) = do
    --   x <- Var (Bound "x" 0) <$> tyInt
    --   pure [x]
    -- patternBinds (VString _) = do
    --   x <- Var (Bound "x" 0) <$> tyString
    --   pure [x]
    -- patternBinds (VData _ vs) = pure vs

    patternBinds (VInt _) = do
      t <- tyInt
      pure [t]
    patternBinds (VString _) = do
      t <- tyString
      pure [t]
    patternBinds (VData cid _) = do
      cinfo <- findDataInfo cid
      pure (cinfo.fields)

    -- filter out repeated test against constructors
    specialize v acc@(d, cs, ms, body) c = do
      if Set.notMember c cs then do
        let mat' = rowSpecialize v c mat
        ts <- patternBinds c
        j <- freshLabel "j" ts
        (vs, e) <- bindVars ((Nothing,) <$> ts) \vs ->
          (vs,) <$> matchCompile mat'
        pure ( d
             , Set.insert c cs
             , ms :|> Alt c (TJmp j vs)
             , TLetK j vs e body
             )
      else pure acc

    -- for each constructor, specialize the pattern matrix
    goHead _ acc@(Just {}, _, _, _) _ = pure acc
    goHead v acc (PInt i)             = specialize v acc (VInt i)
    goHead v acc (PString s)          = specialize v acc (VString s)
    goHead v acc (PData c vs)         = do
      ElabState{localTerms} <- ask
      cinfo <- findDataInfo c
      ((_, localTerms'), vs) <- mapAccumM f
        (0, localTerms)
        (mzip vs cinfo.fields)
      local (\ctx -> ctx
        { termLevel = ctx.termLevel + length vs
        , localTerms = localTerms'
        }) $ specialize v acc (VData c vs)
      where
        f (i, m :: Map Text Var) (PAs x p, t) = do
          let m' = Map.insert x (Var (Bound "x" i) t) m
          case p of
            Just p -> f (i, m') (raisePat p, t)
            Nothing -> pure ((i + 1, m'), Var (Bound "x" i) t)
        f (i, m) (_, t) = pure ((i + 1, m), Var (Bound "x" i) t)

    goRow _  acc@(Just {}, _, _, _) _ = pure acc
    goRow v acc r                     = maybe (pure acc) (goHead v acc) (Map.lookup v r.cols)

localState :: (Applicative m) => (s -> s) -> StateT s m a -> StateT s m a
localState f (StateT k) = StateT \s -> k (f s) <&> _2 .~ s

localState' :: (Applicative m) => StateT s m a -> StateT s m a
localState' = localState id

-- bindPattern :: P.Pattern Range -> Type -> (Pattern 'High -> Var -> Elab r) -> Elab r
-- bindPattern p t k = bindPatterns @Identity (coerce p) (coerce t) (\a b -> k (coerce a) (coerce b))

-- NOTE: we need to provide some way to get variables bound to a pattern or
-- generate a fresh binding
-- NOTE: can we iterate through the data patterns, use the wildcards as we see
-- them and generate fresh names otherwise
-- bindPatterns
--   :: (Traversable t, MonadZip t)
--   => t (P.Pattern Range)
--   -> t Type
--   -> (t (Pattern 'High) -> t Var -> Elab r)
--   -> Elab r
-- bindPatterns ps ts k = do
--   ElabState{localTerms} <- ask
--   let (level, vs) = mapAccumL (\i t -> (i + 1, Var (Bound "x" i) t)) 0 ts
--   (ps, (fmap fst -> bound, _)) <- flip runStateT (mempty, length ps - 1) $
--     sequence $ mzipWith tyCheckPatStep ps ts
--   local (\ctx -> ctx
--     { termLevel = ctx.termLevel + level
--     , localTerms = bound <> localTerms
--     }) (k ps vs)
--   where
--     -- nextVar t = StateT \(m, i) ->
--     --   pure (Var (Bound "x" i) t, (m, i + 1))

--     bindVar (P.Name r n) t = StateT \(m, i) ->
--       pure ((), (Map.insert n (Var (Bound "x" i) t, r) m, i))

--     tyCheckPatStep p t = do
--       p <- tyCheckPat p t
--       StateT \(m, i) -> pure ((), (m, i - 1))
--       pure p

--     tyCheckPat p t = do
--       (p', t') <- tyInferPat p
--       lift $ tyUnify t t' >>= flip unless
--         (tyErrorExpect "pattern" t t' (range p))
--       pure p'

--     tyInferPat (P.PInt _ i)    = do
--       (PInt i,) <$> lift tyInt
--     tyInferPat (P.PString _ s) = do
--       (PString s,) <$> lift tyString
--     tyInferPat (P.PWild _)     = do
--       t <- lift freshMeta
--       pure (PWild (Left t), t)
--     tyInferPat (P.PAs _ n p)   = do
--       m <- gets fst
--       case Map.lookup (P.getName n) m of
--         Nothing -> do
--           t <- lift freshMeta
--           bindVar n t
--           lowerPat <$> tyCheckPat p t >>= \case
--             Nothing -> pure (PWild (Left t), t)
--             Just p -> pure (PWild (Right (P.getName n, p)), t)
--         Just (_, r') -> lift $ throwElab
--           [ "non-linear occurence of variable", errPretty n ]
--           [ (r', This "first defined here"), (P.range n, This "redefined here") ]
--     tyInferPat (P.PType _ p t) = do
--       t <- lift (tyCheckType t)
--       (,t) <$> tyCheckPat p t
--     tyInferPat (P.PData r c ps) = do
--       cid <- lift $ findDataId c
--       cinfo <- lift $ findDataInfo cid
--       let
--         arity = length cinfo.fields
--         arity' = length ps
--       unless (arity == arity') $ lift $ throwElab
--         [ "constructor", errQuote c, "has arity", errPretty arity ]
--         [ (r, This ["used with arity", errPretty arity']) ]
--       ps <- localState' $ sequence $ Seq.zipWith tyCheckPat ps cinfo.fields
--       pure (PData cid ps, TyData cinfo.typeOf)
--     -- tyInferPat (P.POr _ p ps)  = do
--     --   (p', t, m') <- localState' do
--     --     (p', t) <- tyInferPat p
--     --     m <- gets fst
--     --     pure (p', t, m)
--     --   case lowerPat p' of
--     --     Nothing -> pure (p', t)
--     --     Just p' -> do
--     --       (om, ol) <- tyCheckOrPat (range p) m' t ([p'], Nothing) ps
--     --       pure (POr (fromSeq om) ol, t)

--     -- tyCheckOrPat _ _ _ acc Empty                    = pure acc
--     -- -- tyCheckOrPat _ _ _ acc@(_, Just _) _            = pure acc
--     -- tyCheckOrPat r m' t acc (P.POr r' p ps :<| ps') = do
--     --   acc@(_, ol) <- tyCheckOrPat r m' t acc (p :<| ps)
--     --   if isJust ol then pure acc else
--     --     tyCheckOrPat r' m' t acc ps'
--     -- tyCheckOrPat r m' t (om, ol) (p :<| ps) = do
--     --   (p', m'') <- localState' do
--     --     (,) <$> tyCheckPat p t <*> gets fst

--     --   -- TODO: better error messages
--     --   unless (m' == m'') $
--     --     -- NOTE: this is safe since m' and m'' cannot both be empty.
--     --     if length m' > length m'' then do
--     --       let n = Set.findMin (Map.keysSet m' Set.\\ Map.keysSet m'')
--     --       lift $ throwElab
--     --         [ "missing occurence of variable", errPretty n, "in or pattern" ]
--     --         [ (snd $ m' Map.! n, This "first defined here")
--     --         , (range p, This "missing in this alternative")
--     --         ]
--     --     else do
--     --       let n = Set.findMin (Map.keysSet m'' Set.\\ Map.keysSet m')
--     --       lift $ throwElab
--     --         [ "missing occurence of variable", errPretty n, "in or pattern" ]
--     --         [ (snd $ m'' Map.! n, This "first defined here")
--     --         , (r, This "missing in this alternative")
--     --         ]
--     --   case lowerPat p' of
--     --     Nothing -> pure (om, Just p')
--     --     Just p' -> tyCheckOrPat (range p) m' t (om :|> p', ol) ps

tyCheckPat :: P.Pattern Range -> Type -> Elab (Pattern 'High)
tyCheckPat p t = tyCheckPat' p t `evalStateT` mempty

tyInferPat :: P.Pattern Range -> Elab (Pattern High, Type)
tyInferPat p = tyInferPat' p `evalStateT` mempty

tyCheckPat' :: P.Pattern Range -> Type -> StateT (Map Text Range) Elab (Pattern 'High)
tyCheckPat' p t = do
  (p', t') <- tyInferPat' p
  lift $ tyUnify t t' >>= flip unless
    (tyErrorExpect "pattern" t t' (range p))
  pure p'

tyInferPat' :: P.Pattern Range -> StateT (Map Text Range) Elab (Pattern High, Type)
tyInferPat' (P.PType _ p t) = do
  t <- lift (tyCheckType t)
  (,t) <$> tyCheckPat' p t
tyInferPat' (P.PInt _ i)    = do
  (PInt i,) <$> lift tyInt
tyInferPat' (P.PString _ s) = do
  (PString s,) <$> lift tyString
tyInferPat' (P.PWild _)     = do
  t <- lift freshMeta
  pure (PWild t, t)
tyInferPat' (P.PAs _ (P.Name r n) p)   = do
  m <- get
  case Map.lookup n m of
    Nothing -> do
      StateT \m -> pure ((), Map.insert n r m)
      (p, t) <- tyInferPat' p
      pure $ (PAs n (lowerPat p), t)
    Just r' -> lift $ throwElab
      [ "non-linear occurence of variable", errPretty n ]
      [ (r', This "first defined here"), (r, This "redefined here") ]
tyInferPat' (P.PData r c ps) = do
  cid <- lift $ findDataId c
  cinfo <- lift $ findDataInfo cid
  let
    arity = length cinfo.fields
    arity' = length ps
  unless (arity == arity') $ lift $ throwElab
    [ "constructor", errQuote c, "has arity", errPretty arity ]
    [ (r, This ["used with arity", errPretty arity']) ]
  ps <- sequence $ Seq.zipWith tyCheckPat' ps cinfo.fields
  pure (PData cid ps, TyData cinfo.typeOf)

-- bindPattern
--   :: P.Pattern Range
--   -> Var
--   -> (Map Var (Pattern 'Low) -> Seq Var -> Elab r)
--   -> Elab r
-- bindPattern p v@(Var _ t) k = do
--   -- let (level, vs) = mapAccumL (\i t -> (i + 1, Var (Bound "x" i) t)) 0 ts
--   _ <- tyCheckPat p t `evalStateT` mempty >>= \case
--     -- PWild :: Either Type (Text, Pattern 'Low) -> Pattern 'High
--     -- PInt :: Integer -> Pattern l
--     -- PString :: Text -> Pattern l
--     -- PData :: DataId -> Seq (Pattern 'High) -> Pattern l
--     -- POr :: NESeq (Pattern 'Low) -> Maybe (Pattern 'High) -> Pattern 'High
--     _ -> undefined

--   cols <- bindRow False v undefined mempty
--   -- liftIO $ print (v, p, bound)
--   local (\ctx -> ctx
--     { termLevel = length cols + ctx.termLevel
--     -- , localTerms = bound <> ctx.localTerms
--     }) (k cols mempty)
--   where
--     bindRow False _ (PData cid ps) m = do
--       cinfo <- findDataInfo cid
--       let (_, vs) = mapAccumL (\i t -> (i + 1, Var (Bound "x" i) t)) 0 cinfo.fields
--       foldM (\acc (v, p) -> bindRow True v p acc) m (mzip vs ps)
--     bindRow _ v p m =
--       case lowerPat p of
--         Just p' -> pure $ Map.insert v p' m
--         Nothing -> pure m

--     bindVar (P.Name r n) = StateT \m ->
--       pure ((), Map.insert n r m)

data Cont where
  Abstract :: Label -> Cont
  Wrap :: (Var -> Elab Term) -> Cont

contArityError :: HasCallStack => a
contArityError =
  error "abstract continuation called with wrong number of arguments"

apply :: Cont -> Var -> Elab Term
apply (Abstract k@(Label _ [_])) x =  pure $ TJmp k [x]
apply (Abstract _) _               = contArityError
apply (Wrap k) x                   = k x

tyCheckExpr :: HasCallStack => P.Expr Range -> Type -> Cont -> Elab Term
tyCheckExpr e t k = do
  tyInferExpr e $ Wrap \x@(Var _ t') -> do
    tyUnify t t' >>= flip unless
      (tyErrorExpect "expression" t t' e)
    apply k x

tyCheckExprAll
  :: (Foldable f)
  => f (P.Expr Range)
  -> f Type
  -> (Seq Var -> Elab Term)
  -> Elab Term
-- NOTE: this is modified version of foldM
tyCheckExprAll xs ts k = foldr2 c k xs ts []
  where
    {-# INLINE c #-}
    c x t k zs = tyCheckExpr x t (Wrap $ \z -> k (zs :|> z ))

tyInferExpr :: HasCallStack => P.Expr Range -> Cont -> Elab Term
tyInferExpr (P.EInt _ i) k = do
  t <- tyInt
  x <- freshLocal "i" t
  TLetV x (VInt i) <$> apply k x
tyInferExpr (P.EString _ s) k = do
  t <- tyString
  x <- freshLocal "s" t
  TLetV x (VString s) <$> apply k x
tyInferExpr (P.EVar _ v) k = do
  ElabState{termLevel} <- ask
  lookupVar v >>= \case
    Var (Bound x l)  t -> apply k (Var (Bound x (termLevel - l - 1)) t)
    v -> apply k v
-- NOTE: i think disallowing bare primitives is fine, since they are not
-- true functions.
tyInferExpr (P.EPrim r p) k = do
  ElabState{primitives} <- ask
  t <- readIORef primitives >>= Map.lookup (P.getName p) >>> \case
    Nothing -> do
      throwElab
        [ "unknown primitive", errQuote ("#" <> P.getName p) ]
        [ (range p, This "used here") ]
    Just t -> do
      tyZonk t >>= \case
        t@(TyFun {}) -> throwElab
          ["cannot use primitive of type", errPretty t, "as a value"]
          [(r, This "use primitive as value")]
        t -> pure t
  x <- freshLocal "p" t
  TLetP x (PrimOp (P.getName p)) [] <$> apply k x
tyInferExpr (P.EApp r (P.EPrim _ p) xs) k = do
  ElabState{primitives} <- ask
  (args, ret) <- readIORef primitives >>= Map.lookup (P.getName p) >>> \case
    Nothing -> do
      throwElab
        [ "unknown primitive", errQuote ("#" <> P.getName p) ]
        [ (range p, This "used here") ]
    Just t -> do
      tyZonk t >>= \case
        TyFun args ret -> pure (args, ret)
        t -> throwElab
          ["cannot apply primitive of type", errPretty t]
          [(r, This "attempted application")]
  let
    arity = length args
    arity' = length xs
  unless (arity == arity') $ throwElab
    [ "primitive", errQuote ("#" <> P.getName p), "has arity", errPretty arity ]
    [ (range  p, This ["applied to", errPretty arity', "arguments"]) ]
  tyCheckExprAll xs args \xs -> do
    x <- freshLocal "p" ret
    TLetP x (PrimOp (P.getName p)) xs <$> apply k x
tyInferExpr (P.EData r c) k = do
  cid <- findDataId c
  cinfo <- findDataInfo cid
  let
    t = TyData cinfo.typeOf
    arity = length cinfo.fields
  unless (arity == 0) $ throwElab
    [ "constructor", errQuote c, "has arity", errPretty arity ]
    [ (r, This "used with arity 0") ]
  x <- freshLocal "d" t
  TLetV x (VData cid []) <$> apply k x
tyInferExpr (P.EApp r (P.EData _ c) xs) k = do
  cid <- findDataId c
  cinfo <- findDataInfo cid
  let
    t = TyData cinfo.typeOf
    arity = length cinfo.fields
    arity' = length xs
  unless (arity == arity') $ throwElab
    [ "constructor", errQuote c, "has arity", errPretty arity ]
    [ (r, This ["used with arity", errPretty arity']) ]
  tyCheckExprAll xs cinfo.fields \xs -> do
    x <- freshLocal "d" t
    TLetV x (VData cid xs) <$> apply k x
tyInferExpr e@(P.EApp r f xs) k =
  tyInferExpr f $ Wrap \f@(Var fn ft) -> do
    (args, ret) <- tyForce ft >>= \case
      TyFun args ret -> pure (args, ret)
      _ -> throwElab
        ["cannot apply expression of type", errPretty ft]
        [(r, This "attempted application")]
    let
      arity = length args
      arity' = length xs
    unless (arity == arity') $ throwElab
      [ "function", errQuote fn, "has arity", errPretty arity ]
      [ (r, This ["applied to", errPretty arity', "arguments"]) ]
    tyCheckExprAll xs args \xs -> do
      case k of
        Abstract k@(Label _ [t]) -> do
          -- TODO: are error messages talking about the right thing?
          tyUnify t ret >>= flip unless
            (tyErrorExpect "expression" t ret e)
          pure $ TApp f k xs
        Abstract _ -> contArityError
        Wrap k     -> do
          (vs, kx) <- bindVar (Nothing, ret) \v -> ([v],) <$> k v
          k <- freshLabel "k" [ret]
          pure $ TLetK k vs kx (TApp f k xs)
tyInferExpr (P.ELet _ (P.ExprDecl _ x t Empty e1) e2) k = do
  t <- maybe freshMeta tyCheckType t
  j <- freshLabel "j" [t]
  (vs, e2) <- bindVar (Just x, t) \v ->
    ([v],) <$> tyInferExpr e2 k
  TLetK j vs e2 <$> tyCheckExpr e1 t (Abstract j)
-- tyInferExpr (P.ELet _ (P.ExprDecl _ (P.Name _ f) t ps e1) e2) k = do
--   t <- maybe freshMeta tyCheckType t
--   ts <- traverse (const freshMeta) ps
--   f' <- freshLocal f (TyFun ts t)
--   -- TODO: audit this use of bind patterns
--   e1K <- bindPatterns ps ts \_ps vs -> do
--     -- HACK: i don't think we are binding the right parameters for k
--     k <- freshLabel "k" [t]
--     TLetF f' k vs <$> tyCheckExpr e1 t (Abstract k)
--   local (\ctx -> ctx
--       { localTerms = Map.insert f f' ctx.localTerms
--       }) $ e1K <$> tyInferExpr e2 k
tyInferExpr (P.EMatch _ e alts) k = do
  tyInferExpr e $ Wrap \x -> do
    case k of
      Abstract (Label _ [t]) -> mkMatrix x t k
      Abstract _ -> contArityError
      Wrap k     -> do
        t <- freshMeta
        kx <- bindVar (Nothing, t) k
        k <- freshLabel "k" [t]
        TLetK k [x] kx <$>
          mkMatrix x t (Abstract k)
  where
    mkMatrix x t k = do
      -- NOTE: mkRow never evalutates `body`
      (Matrix -> m) <- foldM (mkRow x t k) Empty alts
      matchCompile m

    -- mkRow x t k (body, rows) (P.Alt _ p _g e) =
    --   bindPattern p x \cols vs -> do
    --     e <- tyCheckExpr e t k
    --     k <- freshLabel "k" $ (\(Var _ t) -> t) <$> vs
    --     pure (TLetK k vs e body, Row
    --      { cols
    --      , guard = Nothing
    --      , body = TJmp k vs
    --      } :<| rows)
    mkRow x@(Var _ pt) t k (rows) (P.Alt _ p _g e) = do
      cols <- tyCheckPat p pt <&> lowerPat >>> \case
        Nothing -> mempty
        Just p -> Map.singleton x p
      pure $ rows :|> Row
       { cols
       , guard = Nothing
       , body = tyCheckExpr e t k
       }
tyInferExpr _ _ = error "TODO"

-- TODO: how do i know if this is a value? probably some check of triviality
tyInferExprDecl :: HasCallStack => P.ExprDecl Range -> Elab Decl
-- tyInferExprDecl (P.ExprDecl _r (P.Name _ f) t ps e) = do
--   ts <- traverse (const freshMeta) ps
--   t <- maybe freshMeta tyCheckType t
--   k <- freshLabel "k" [t]
--   f <- freshGlobal f (TyFun ts t)
--   (ps, vs, ek) <- bindPatterns ps ts \ps vs -> do
--     liftIO $ putStr "\t" >> print ps
--     liftIO $ putStr "\t" >> print vs
--     e <- tyCheckExpr e t (Abstract k)
--     k <- freshLabel "k" $ (\(Var _ t) -> t) <$> vs
--     pure $ (ps, vs, TLetK k vs e)

--   -- pure $ DTerm f k vs e
--   error "TODO"
tyInferExprDecl (P.ExprDecl _r (P.Name _ f) t ps e) = do
  ts <- traverse (const freshMeta) ps
  t <- maybe freshMeta tyCheckType t
  k <- freshLabel "k" [t]
  f <- freshGlobal f (TyFun ts t)
  bindVars ((Nothing,) <$> ts) \vs -> do
    -- liftIO do
    --   print (void d)
    --   putStr "\t" >> print _ps
    --   putStr "\t" >> print vs
    --   putStrLn ""
    e <- tyCheckExpr e t (Abstract k)
    pure $ DTerm f k vs e

elaborate :: HasCallStack => P.Module -> Elab (Seq Decl)
elaborate (P.Module ds) = do
  -- get the names of all data types
  forM_ ds \case
    P.DData _ m c _ -> do
      makeTypeId m c $> ()
    _ -> pure ()
  ElabState{primitives} <- ask
  exprs <- (`witherM` ds) \case
    P.DData _ m c xs -> tyCheckDataDecl m c xs $> Nothing
    P.DPrim _ (P.Name r n) t -> do
      readIORef primitives >>= Map.lookup n >>> \case
        Just _ -> do
          throwElab
            [ "redefinition of primitive", errQuote ("#" <> n) ]
            [ (r, This "redefined here") ]
        Nothing -> do
          t <- tyCheckType t
          modifyIORef primitives (Map.insert n t)
      pure Nothing
    P.DExpr e -> pure (Just e)

  mapM tyInferExprDecl exprs >>= zonk
