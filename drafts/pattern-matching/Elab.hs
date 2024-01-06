{-# OPTIONS_GHC -Wno-unused-top-binds   #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoFieldSelectors           #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Elab
    ( Elab
    , runElab
    , elaborate
    ) where

import Control.Applicative
import Control.Category     qualified ((>>>))
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Core                 as C
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import Data.IntMap.Strict   (IntMap)
import Data.IntMap.Strict   qualified as IntMap
import Data.IORef           (IORef)
import Data.IORef           qualified as IORef
import Data.Map.Strict      (Map)
import Data.Map.Strict      qualified as Map
import Data.Sequence        (Seq (..))
import Data.Sequence        qualified as Seq
import Data.Set             (Set)
import Data.Set             qualified as Set
import Data.Text            (Text)
import Error
import GHC.Exts             (oneShot)
import GHC.Stack
import Lens.Micro
import Parser               qualified as P
import Prettyprinter        qualified as Pretty
import Unsafe.Coerce
import Witherable

newtype RecFold a b
  = MkRecFold { fold :: a -> (RecFold a b -> b) -> b }

pattern RecFold :: (a -> (RecFold a b -> b) -> b) -> RecFold a b
pattern RecFold f <- MkRecFold f
  where RecFold f = MkRecFold (oneShot f)
{-# COMPLETE RecFold #-}

foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2 c i xs =
  foldr f (const i) xs . RecFold . foldr g (\_ _ -> i) where
    g e2 r2 e1 r1 = c e1 e2 (r1 (RecFold r2))
    f e r (RecFold f) = f e r
{-# INLINEABLE foldr2 #-}

newIORef :: (MonadIO m) => x -> m (IORef x)
newIORef = liftIO . IORef.newIORef

readIORef :: (MonadIO m) => IORef x -> m x
readIORef = liftIO . IORef.readIORef

writeIORef :: (MonadIO m) => IORef x -> x -> m ()
writeIORef r = liftIO . IORef.writeIORef r

-- modifyIORef' :: (MonadIO m) => IORef x -> (x -> x) -> m ()
-- modifyIORef' k = liftIO . IORef.modifyIORef k

modifyIORef :: (MonadIO m) => IORef x -> (x -> x) -> m ()
modifyIORef k = liftIO . IORef.modifyIORef' k

data ElabState = ElabState
  { fileName     :: FilePath
  , fresh        :: IORef Int
  , types        :: IORef (Map Text TypeId)
  , typeInfo     :: IORef (IntMap TypeInfo)
  , dataCons     :: IORef (Map Text DataId)
  , dataConInfo  :: IORef (IntMap DataInfo)
  , globalTerms  :: IORef (Map Text Var)
  , localTerms   :: Map Text Var
  , termLevel    :: Int
  , patternLevel :: Int
  }

newtype Elab s
  = Elab (ReaderT ElabState IO s)
  deriving
    ( Alternative
    , Applicative
    , Functor
    , Monad
    , MonadFail
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
    <*> pure mempty
    <*> pure 0
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

tyUnify :: Type -> Type -> Elab Bool
tyUnify ty ty' | ty == ty' = pure True
tyUnify (TyMeta _ (Trivial ty)) ty' = do
  readIORef ty >>= \case
    Just ty -> tyUnify ty ty'
    Nothing ->
      -- TODO: occurs check
      writeIORef ty (Just ty') $> True
tyUnify ty ty'@(TyMeta {}) = tyUnify ty' ty
tyUnify (TyFun xs x) (TyFun ys y) = do
  fail <- tyUnify x y
  go fail xs ys
  where
    go True (x :<| xs) (y :<| ys) = do
      stop <- tyUnify x y
      go stop xs ys
    go r _ _ = pure r
tyUnify _ _ = pure False

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

tyCheckDataDecl :: HasCallStack => P.Name Range -> Seq (P.DataCon Range) -> Elab ()
tyCheckDataDecl t cs = do
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
      TypeInfo { name = tid, range = range t, span = length cs }
  forM_ cs \(P.DataCon _ name xs) -> do
    DataId did <- makeDataId name
    -- TODO: allow function types in data types
    fields <- fmap TyData <$> mapM findTypeId xs
    modifyIORef dataConInfo . IntMap.insert did.id $
      DataInfo { name = did, range = range t, typeOf = TypeId tid, .. }

tyMagic :: Text -> Elab Type
tyMagic n = TyData <$> makeTypeId True (P.Name () n)

tyInt :: Elab Type
tyInt = tyMagic "Int"

tyString :: Elab Type
tyString = tyMagic "String"

data Cont where
  Abstract :: Label -> Cont
  Wrap :: (Var -> Elab (Term, Type)) -> Cont

contArityError :: HasCallStack => a
contArityError =
  error "abstract continuation called with wrong number of arguments"

apply :: Cont -> Var -> Elab Term
apply (Abstract k@(Label _ [t])) x@(Var _ t') = do
  -- TODO: do we actually need this unification?
  -- TODO: check return value of unification
  tyUnify t t' $> TJmp k [x]
apply (Abstract _) _ = contArityError
apply (Wrap k) x = fst <$> k x

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

withBound :: Type -> (Var -> Elab r) -> Elab r
withBound t k = do
  ElabState{termLevel} <- ask
  let v = Var (Bound "x" termLevel) t
  local (\ctx -> ctx
    { termLevel = termLevel + 1
    }) (k v)

freshBound :: Type -> Cont -> Elab Term
freshBound t k = do
  ElabState{termLevel} <- ask
  let v = Var (Bound "x" termLevel) t
  case k of
    Abstract k -> pure $ TJmp k [v]
    Wrap k ->
      local (\ctx -> ctx
        { termLevel = termLevel + 1
        }) (fst <$> k v)

tyForce :: Type -> Elab Type
tyForce (TyMeta i (Trivial m)) = go i m
  where
    go i m = readIORef m >>= \case
      Just (TyMeta i (Trivial m')) -> do
        m' <- go i m'
        writeIORef m (Just m')
        pure m'
      Just m -> pure m
      _ -> pure (TyMeta i (Trivial m))
tyForce t = pure t

tyZonk :: Type -> Elab Type
tyZonk t@(TyMeta _ (Trivial m)) =
  readIORef m >>= \case
    Just m -> tyZonk m
    _ -> pure t
tyZonk (TyFun a b)  = TyFun <$> traverse tyZonk a <*> tyZonk b
tyZonk x            = pure x

tyCheckExpr :: HasCallStack => P.Expr Range -> Type -> Cont-> Elab Term
tyCheckExpr e t k = do
  (e', t') <- tyInferExpr e k
  u <- tyUnify t t'
  unless u $
    tyErrorExpect "expression" t t' e
  pure e'

tyCheckExprAll
  :: (Foldable f)
  => f (P.Expr Range)
  -> f Type
  -> (Seq Var -> Elab (Term, Type))
  -> Elab (Term, Type)
-- NOTE: this is modified version of foldM
tyCheckExprAll xs ts k = foldr2 c k xs ts []
  where
    {-# INLINE c #-}
    c x t k zs = (,t)
      <$> tyCheckExpr x t (Wrap $ \z -> k (zs :|> z ))

tyInferExpr :: HasCallStack => P.Expr Range -> Cont -> Elab (Term, Type)
tyInferExpr (P.EInt _ i) k = do
  t <- tyInt
  x <- freshLocal "l" t
  e <- apply k x
  pure (TLetV x (VInt i) e, t)
tyInferExpr (P.EString _ s) k = do
  t <- tyString
  x <- freshLocal "s" t
  e <- apply k x
  pure (TLetV x (VString s) e, t)
tyInferExpr (P.EVar _ v) k = do
  x@(Var _ t) <- lookupVar v
  e <- apply k x
  pure (e, t)
tyInferExpr (P.EApp _ (P.EPrim _ _) _) _ = error "TODO: prims"
tyInferExpr (P.EPrim {}) _ = error "type error"
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
    e <- apply k x
    pure (TLetV x (VData cid xs) e, t)
tyInferExpr (P.EData {}) _ = error "type error"
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
          u <- tyUnify t ret
          unless u $
            tyErrorExpect "expression" t ret e
          pure (TApp f k xs, ret)
        Abstract _ -> contArityError
        Wrap _     -> do
          kx <- freshBound ret k
          k <- freshLabel "k" [ret]
          pure (TLetK k 1 kx (TApp f k xs), ret)
tyInferExpr (P.ELet _ (P.ExprDecl _ (P.Name _ x) Empty e1) e2) k = do
  t <- freshMeta
  (e2', t2) <- withBound t \v ->
    local (\ctx -> ctx
      { localTerms = Map.insert x v ctx.localTerms
      }) $ tyInferExpr e2 k
  j <- freshLabel "j" [t]
  (e1', t1) <- tyInferExpr e1 (Abstract j)
  u <- tyUnify t t1
  unless u $
    tyErrorExpect "expression" t t1 e1
  pure (TLetK j 1 e2' e1', t2)
tyInferExpr (P.ELet _ (P.ExprDecl _ _ _ _) _) _ = do
  error "TODO"
tyInferExpr (P.EMatch {}) _ = do
  error "TODO"

tyCheckExprDecl :: HasCallStack => P.ExprDecl Range -> Elab a
tyCheckExprDecl (P.ExprDecl _r _f _ps _e) = do
  -- ps <- mapM tyInferPat ps
  -- e <- tyInferExpr e
  -- let ty = snd $ info e
  -- pure $ ExprDecl (r, ty) ((,ty) <$> f) ps e
  error "TODO"

elaborate :: HasCallStack => P.Module -> Elab ()
elaborate (P.Module ds) = do
  forM_ ds \case
    P.DData _ c m _ -> makeTypeId m c $> ()
    _ -> pure ()
  exprs <- (`witherM` ds) \case
    P.DData _ c _ xs -> tyCheckDataDecl c xs $> Nothing
    P.DExpr e -> pure (Just e)
  mapM_ tyCheckExprDecl exprs
  pure ()

data PatLevel
  = Low
  | High

data NESeq x = NESeq x (Seq x)
  deriving (Foldable)

data Pattern l where
  PInt :: Integer -> Pattern l
  PString :: Text -> Pattern l
  PData :: DataId -> Seq (Pattern 'High) -> Pattern l
  POr :: Map AltCon (Seq (Pattern 'High)) -> Maybe (Pattern 'High) -> Pattern l
  PWild :: Maybe Name -> Pattern 'High
  PAs :: Name -> Pattern 'Low -> Pattern 'High

lower :: Pattern l -> (Maybe (Pattern 'Low), Maybe Name)
lower (PInt i)     = (Just $ (PInt i), Nothing)
lower (PString i)  = (Just $ (PString i), Nothing)
lower (PData c ps) = (Just $ (PData c ps), Nothing)
lower (POr qs p)   = (Just $ (POr qs p), Nothing)
lower (PWild x)    = (Nothing, x)
lower (PAs x p)    = (Just $ p, Just x)

raise :: Pattern l -> Pattern 'High
raise p = unsafeCoerce p

data MatchRow = Row
  { cols  :: Map Var (Pattern 'Low)
  , guard :: Maybe (P.Expr Range, Pattern 'Low)
  , vars  :: Map Text Var
  , body  :: Label
  }

rColsL :: Lens' MatchRow (Map Var (Pattern 'Low))
rColsL f x = (\x' -> x {cols = x'}) <$> f x.cols

rVarsL :: Lens' MatchRow (Map Text Var)
rVarsL f x = (\x' -> x {vars = x'}) <$> f x.vars

rowMatchVar :: Var -> Pattern 'Low -> MatchRow -> MatchRow
rowMatchVar x p = rColsL %~ Map.insert x p

rowBindVar :: Text -> Var -> MatchRow -> MatchRow
rowBindVar x v = rVarsL %~ Map.insert x v

newtype PatternMatrix
  = Matrix { rows :: Seq MatchRow }

handlePattern
  :: Var
  -> Pattern l
  -> MatchRow
  -> (MatchRow -> b)
  -> b
handlePattern var pat row f = do
  let (pat', bound) = lower pat
      row'          = maybe row (\x -> rowBindVar x var row) (undefined bound)
    in f $ case pat' of
      Nothing   -> row' & rColsL %~ Map.delete var
      Just pat' -> rowMatchVar var pat' row'

patternAltCons :: Pattern l -> Set AltCon
patternAltCons = go mempty
  where
    go :: Set AltCon -> Pattern l -> Set AltCon
    go acc (PData (AltData -> c) _) | c `Set.notMember` acc = Set.insert c acc
    go acc (POr qs p) = Map.keysSet qs <> maybe acc (go acc) p
    go acc (PAs _ p) = go acc p
    go acc _ = acc

rowDefault :: Var -> PatternMatrix -> PatternMatrix
rowDefault var mat = mat { rows = foldl' f Empty mat.rows }
  where
    hasWildCard :: Pattern l -> Bool
    hasWildCard (PWild {})        = True
    hasWildCard (PAs _ p)         = hasWildCard p
    hasWildCard (POr _ (Just p )) = hasWildCard p
    hasWildCard _                 = False

    f acc row = case Map.lookup var row.cols of
      -- add the row as is since its a wildcard
      Nothing                               -> acc :|> row
      -- we only need to look at the last pattern to decide
      -- whether we should add an or pattern row
      Just (POr _ (Just p)) | hasWildCard p -> acc :|> row
      -- delete every other row
      Just _                                -> acc

rowSpecialize :: Var -> AltCon -> Seq Var -> PatternMatrix -> PatternMatrix
rowSpecialize var alt conVars mat = mat { rows = foldl' f Empty mat.rows }
  where
    k :: MatchRow -> Seq (Pattern l) -> MatchRow
    k row =
      let g row i p = handlePattern (conVars `Seq.index` i) p row id
       in Seq.foldlWithIndex g (row & rColsL %~ Map.delete var)

    g :: Pattern l -> MatchRow -> MatchRow
    g (PAs _ p) row = g p row
    g (PData _ args) row = k row args
    g (POr ps _) row | Just args <- Map.lookup alt ps = k row args
    g (POr _ (Just p)) row = g p row
    g _ _ = error "unreachable"

    f acc row = case (alt, Map.lookup var row.cols) of
      -- add the row as is since it's wildcard
      -- because it places no constraints on `var`
      (_, Nothing) -> acc :|> row
      (AltInt i, Just (PInt i')) | i == i' ->
        -- delete the column since there are no sub patterns
        acc :|> (row & rColsL %~ Map.delete var)
      (AltString s, Just (PString s')) | s == s' ->
        -- delete the column since there are no sub patterns
        acc :|> (row & rColsL %~ Map.delete var)
      -- otherwise replace the column with its sub patterns
      (AltData con, Just (PData con' ps)) | con == con'->
        let g row i p = handlePattern (conVars `Seq.index` i) p row id in
          (acc :|>) $ Seq.foldlWithIndex
            g (row & rColsL %~ Map.delete var) ps
      -- since the constrcutors don't match delete the row
      (_, Just p) | alt `Set.member` patternAltCons p ->
        acc :|> g p row
      _ -> acc
