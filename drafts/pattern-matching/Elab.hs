{-# OPTIONS_GHC -Wno-unused-top-binds   #-}
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
    , runElab
    , elaborate
    ) where

import Control.Applicative
import Control.Category           qualified ((>>>))
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State.Strict
import Core                       as C
import Data.Bifunctor
import Data.Bool                  (bool)
import Data.Foldable
import Data.Functor
import Data.Functor.Identity
import Data.IntMap.Strict         (IntMap)
import Data.IntMap.Strict         qualified as IntMap
import Data.IORef                 (IORef)
import Data.IORef                 qualified as IORef
import Data.List                  qualified as List
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Maybe                 (isJust)
import Data.Sequence              (Seq (..))
import Data.Sequence              qualified as Seq
import Data.Set                   (Set)
import Data.Set                   qualified as Set
import Data.Text                  (Text)
import Error
import GHC.Exts                   (coerce, oneShot)
import GHC.Stack
import Lens.Micro
import Parser                     qualified as P
import Prettyprinter              qualified as Pretty
-- import Prettyprinter.Render.Text  qualified as Pretty
-- import Unsafe.Coerce
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

foldM2
  :: (Monad m, Foldable f, Foldable g)
  => (c -> a -> b -> m c) -> c -> f a -> g b -> m c
foldM2 f z0 xs ys = foldr2 c return xs ys z0
  where c x y k z = f z x y >>= k; {-# INLINE c #-}
{-# INLINEABLE foldM2 #-}

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

withBound' :: (Traversable t) => t Type -> (t Var -> Elab r) -> Elab r
withBound' ts k = do
  ElabState{termLevel} <- ask
  let
    (vs, termLevel') = flip runState termLevel $
      mapM (\t -> state \i -> (Var (Bound "x" i) t, i + 1)) ts
  local (\ctx -> ctx
    { termLevel = termLevel'
    }) (k vs)

withBound :: Type -> (Var -> Elab r) -> Elab r
withBound t k = withBound' @Identity (coerce t) (k . coerce)

data Level
  = Low
  | High

data NESeq x = NESeq x (Seq x)
  deriving (Eq, Foldable, Ord)

fromSeq :: HasCallStack => Seq a -> NESeq a
fromSeq Empty      = error "empty Seq"
fromSeq (x :<| xs) = NESeq x xs

data Pattern l where
  PWild :: Var -> Pattern 'High
  PInt :: Integer -> Pattern l
  PString :: Text -> Pattern l
  PData :: DataId -> Seq (Pattern 'High) -> Pattern l
  POr :: NESeq (Pattern 'Low) -> Maybe (Pattern 'High) -> Pattern l

deriving instance Eq (Pattern l)
deriving instance Ord (Pattern l)

lowerPat :: Pattern l -> Maybe (Pattern 'Low)
lowerPat (PInt i)     = Just $ PInt i
lowerPat (PString i)  = Just $ PString i
lowerPat (PData c ps) = Just $ PData c ps
lowerPat (POr qs p)   = Just $ POr qs p
lowerPat _            = Nothing

-- raise :: Pattern l -> Pattern 'High
-- raise p = unsafeCoerce p

-- patternAltCons :: Pattern l -> Set AltCon
-- patternAltCons = go mempty
--   where
--     go :: Set AltCon -> Pattern l -> Set AltCon
--     go acc (PData (AltData -> c) _) | c `Set.notMember` acc = Set.insert c acc
--     go acc (POr qs p) = Map.keysSet qs <> maybe acc (go acc) p
--     go acc (PAs _ p) = go acc p
--     go acc _ = acc

-- patternTypeInfo :: Pattern 'Low -> Elab TypeInfo
-- patternTypeInfo (PInt _) = makeTypeId True (P.Name () "Int")
--   >>= findTypeInfo
-- patternTypeInfo (PString _) = makeTypeId True (P.Name () "String")
--   >>= findTypeInfo
-- patternTypeInfo (PData did _) = do
--   dinfo <- findDataInfo did
--   findTypeInfo dinfo.typeOf
-- patternTypeInfo (POr ps _ )               =
--   let (c, _) = Map.findMin ps in
--   case c of
--     AltInt _ -> do
--       makeTypeId True (P.Name () "Int")
--         >>= findTypeInfo
--     AltString _ -> do
--       makeTypeId True (P.Name () "String")
--         >>= findTypeInfo
--     AltData did _ -> do
--       dinfo <- findDataInfo did
--       findTypeInfo dinfo.typeOf

localState :: (Applicative m) => (s -> s) -> StateT s m a -> StateT s m a
localState f (StateT k) = StateT \s -> k (f s) <&> _2 .~ s

localState' :: (Applicative m) => StateT s m a -> StateT s m a
localState' = localState id

withPattern :: P.Pattern Range -> Type -> (Pattern 'High -> Seq Var -> Elab r) -> Elab r
withPattern p t k = do
  ElabState{termLevel,localTerms} <- ask
  ((fmap fst -> bound, p), termLevel') <- flip runStateT termLevel $
    tyCheckPat mempty p t
  local (\ctx -> ctx
    { termLevel = termLevel'
    , localTerms = bound <> localTerms
    }) (k p $ Seq.fromList . List.sort $ Map.elems bound)
  where
    nextVar t = StateT \i -> pure (Var (Bound "x" i) t, i + 1)

    tyCheckPat m p t = do
      (m, t', p') <- tyInferPat m p
      lift $ tyUnify t t' >>= flip unless
        (tyErrorExpect "pattern" t t' (range p))
      pure (m, p')

    tyInferPat m (P.PInt _ i)    = do
      t <- lift tyInt
      pure (m, t, PInt i)
    tyInferPat m (P.PString _ s) = do
      t <- lift tyString
      pure (m, t, PString s)
    tyInferPat m (P.PWild _)     = do
      t <- lift freshMeta
      v <- nextVar t
      pure (m, t, PWild v)
    tyInferPat m (P.PAs _ (P.Name r n) p) =
      case Map.lookup n m of
        Nothing -> do
          -- TODO: can we clean this up?
          t <- lift freshMeta
          v <- nextVar t
          (m, p) <- tyCheckPat (Map.insert n (v, r) m) p t
          pure (m, t, p)
        Just (_, r') -> lift $ throwElab
          [ "non-linear occurence of variable", errPretty n ]
          [ (r', This "first defined here"), (r, This "redefined here") ]
    tyInferPat m (P.PType _ p t) = do
      t <- TyData <$> lift (findTypeId t)
      (m, p) <- tyCheckPat m p t
      pure (m, t, p)
    tyInferPat m (P.PData r c ps) = do
      cid <- lift $ findDataId c
      cinfo <- lift $ findDataInfo cid
      let
        arity = length cinfo.fields
        arity' = length ps
      unless (arity == arity') $ lift $ throwElab
        [ "constructor", errQuote c, "has arity", errPretty arity ]
        [ (r, This ["used with arity", errPretty arity']) ]
      -- TODO: when allocating variables, should we allocate for all fields of a
      -- constructor, even if the field is never named? i think this makes
      -- consistent naming easier later on
      -- modify' (+ length cinfo.fields)
      (m, ps) <- foldM2 (\(m, ps) p t -> do
          (m, p) <- tyCheckPat m p t
          pure (m, p :<| ps)
        ) (m, Empty) ps cinfo.fields
      pure (m, TyData cinfo.typeOf, PData cid ps)
    tyInferPat m (P.POr _ p ps)  = do
      (m', t, p') <- localState' $ tyInferPat m p
      case lowerPat p' of
        Nothing -> pure (m', t, p')
        Just p' -> do
          (om, ol) <- tyCheckOrPat (range p) m m' t ([p'], Nothing) ps
          pure (m', t, POr (fromSeq om) ol)

    tyCheckOrPat _ _ _ _ acc Empty                   = pure acc
    -- tyCheckOrPat _ _ _ acc@(_, Just _) _           = pure acc
    tyCheckOrPat r m m' t acc (P.POr r' p ps :<| ps') = do
      acc@(_, ol) <- tyCheckOrPat r m m' t acc (p :<| ps)
      if isJust ol then pure acc else
        tyCheckOrPat r' m m' t acc ps'
    tyCheckOrPat r m m' t (om, ol) (p :<| ps) = do
      (m'', p') <- tyCheckPat m p t
      -- TODO: better error messages
      unless (m' == m'') $
        -- NOTE: this is safe since m' and m'' cannot both be empty.
        if length m' > length m'' then do
          let n = Set.findMin (Map.keysSet m' Set.\\ Map.keysSet m'')
          lift $ throwElab
            [ "missing occurence of variable", errPretty n, "in or pattern" ]
            [ (snd $ m' Map.! n, This "first defined here")
            , (range p, This "missing in this alternative")
            ]
        else do
          let n = Set.findMin (Map.keysSet m'' Set.\\ Map.keysSet m')
          lift $ throwElab
            [ "missing occurence of variable", errPretty n, "in or pattern" ]
            [ (snd $ m'' Map.! n, This "first defined here")
            , (r, This "missing in this alternative")
            ]
      case lowerPat p' of
        Nothing -> pure (om, Just p')
        Just p' -> tyCheckOrPat (range p) m m' t (om :|> p', ol) ps

data Cont where
  Abstract :: Label -> Cont
  Wrap :: (Var -> Elab (Term, Type)) -> Cont

contArityError :: HasCallStack => a
contArityError =
  error "abstract continuation called with wrong number of arguments"

apply :: Cont -> Var -> Elab Term
apply (Abstract k@(Label _ [_])) x =  pure $ TJmp k [x]
apply (Abstract _) _               = contArityError
apply (Wrap k) x                   = fst <$> k x

freshBound :: Type -> Cont -> Elab (Term, Var)
freshBound t k = do
  ElabState{termLevel} <- ask
  let v = Var (Bound "x" termLevel) t
  case k of
    Abstract k -> pure (TJmp k [v], v)
    Wrap k ->
      local (\ctx -> ctx
        { termLevel = termLevel + 1
        }) ((_2 .~ v) <$> k v)

tyCheckExpr :: HasCallStack => P.Expr Range -> Type -> Cont-> Elab Term
tyCheckExpr e t k = do
  fmap fst . tyInferExpr e $ Wrap \x@(Var _ t') -> do
    tyUnify t t' >>= flip unless
      (tyErrorExpect "expression" t t' e)
    (,t') <$> apply k x

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

-- TODO: this is all wrong. if i have some int `i` that gets converted to
-- so some `k i` where `k` never returns. it does not have type `i`.
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
tyInferExpr (P.EPrim {}) _ = error "type error"
tyInferExpr (P.EApp _ (P.EPrim _ _) _) _ = error "TODO: prims"
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
  e <- apply k x
  pure (TLetV x (VData cid []) e, t)
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
          pure (TApp f k xs, ret)
        Abstract _ -> contArityError
        Wrap _     -> do
          (kx, v) <- freshBound ret k
          k <- freshLabel "k" [ret]
          pure (TLetK k [v] kx (TApp f k xs), ret)
tyInferExpr (P.ELet _ (P.ExprDecl _ (P.Name _ x) Empty e1) e2) k = do
  t <- freshMeta
  j <- freshLabel "j" [t]
  (e1', t1) <- tyInferExpr e1 (Abstract j)
  tyUnify t t1 >>= flip unless
    (tyErrorExpect "expression" t t1 e1)
  withBound t \v -> do
    (e2', t2) <- local (\ctx -> ctx
      { localTerms = Map.insert x v ctx.localTerms
      }) $ tyInferExpr e2 k
    pure (TLetK j [v] e2' e1', t2)
tyInferExpr (P.ELet _ (P.ExprDecl _ _f _ps _e1) _e2) _k = do
  error "TODO"
tyInferExpr (P.EMatch _ e alts) k = do
  tyInferExpr e $ Wrap \x -> do
    case k of
      Abstract (Label _ [t]) -> mkMatrix x t k
      Abstract _ -> contArityError
      Wrap _     -> do
        t <- freshMeta
        (kx, x) <- freshBound t k
        k <- freshLabel "k" [t]
        mkMatrix x t (Abstract k)
          <&> _1 %~ TLetK k [x] kx
  where
    mkMatrix x t k = mdo
      -- NOTE: mkRow never evalutates `body`
      (letKs, Matrix -> m) <- foldM (mkRow x t k) (body, Empty) alts
      body <- matchCompile m
      pure (letKs, t)

    mkRow x@(Var _ xt) t k (body, rows) (P.Alt _ p _g e) =
      withPattern p xt \p vs -> do
        e <- tyCheckExpr e t k
        k <- freshLabel "k" $ (\(Var _ t) -> t) <$> vs
        pure (TLetK k vs e body, Row
         { cols = maybe [] (\p -> [(x,p)]) (lowerPat p)
         , guard = Nothing
         , binds = vs
         , body = TJmp k vs
         } :<| rows)

-- tyCheckExprDecl :: HasCallStack => P.ExprDecl Range -> Elab a
tyInferExprDecl (P.ExprDecl _r _f _ps e) = do
  -- ps <- mapM tyInferPat ps
  t <- freshMeta
  k <- freshLabel "k" [t]
  tyCheckExpr e t (Abstract k)
  -- let ty = snd $ info e
  -- pure $ ExprDecl (r, ty) ((,ty) <$> f) ps e

elaborate :: HasCallStack => P.Module -> Elab ()
elaborate (P.Module ds) = do
  forM_ ds \case
    P.DData _ c m _ -> do
      makeTypeId m c $> ()
    _ -> pure ()
  exprs <- (`witherM` ds) \case
    P.DData _ c m xs -> tyCheckDataDecl m c xs $> Nothing
    P.DExpr e -> pure (Just e)

-- mapM_ tyCheckExprDecl exprs
  forM_ exprs $ \x -> do
    -- liftIO $ Pretty.putDoc (Pretty.pretty x) >> putChar '\n'
    liftIO $ print (void x)
    x <- tyInferExprDecl x
    liftIO $ print x >> putChar '\n'

data MatchRow = Row
  { cols  :: Map Var (Pattern 'Low)
  , guard :: Maybe (P.Expr Range, Pattern 'Low)
  , binds :: Seq Var
  , body  :: Term
  }

-- rColsL :: Lens' MatchRow (Map Var (Pattern 'Low))
-- rColsL f x = (\x' -> x {cols = x'}) <$> f x.cols

-- rVarsL :: Lens' MatchRow (Map Text Var)
-- rVarsL f x = (\x' -> x {vars = x'}) <$> f x.vars

-- rowMatchVar :: Var -> Pattern 'Low -> MatchRow -> MatchRow
-- rowMatchVar x p = rColsL %~ Map.insert x p

-- rowBindVar :: Text -> Var -> MatchRow -> MatchRow
-- rowBindVar x v = rVarsL %~ Map.insert x v

newtype PatternMatrix
  = Matrix { rows :: Seq MatchRow }

-- handlePattern
--   :: Var
--   -> Pattern l
--   -> MatchRow
--   -> (MatchRow -> b)
--   -> b
-- handlePattern var pat row f = do
--   let (pat', bound) = lower pat
--       row'          = maybe row (\x -> rowBindVar x var row) bound
--     in f $ case pat' of
--       Nothing   -> row' & rColsL %~ Map.delete var
--       Just pat' -> rowMatchVar var pat' row'

rowDefault :: Var -> PatternMatrix -> PatternMatrix
rowDefault var mat = mat { rows = foldl' f Empty mat.rows }
  where
    hasWildCard :: Pattern l -> Bool
    hasWildCard (PWild {})        = True
    -- hasWildCard (PAs _ p)         = hasWildCard p
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

-- rowSpecialize :: Var -> AltCon -> Seq Var -> PatternMatrix -> PatternMatrix
-- rowSpecialize var alt conVars mat = mat { rows = foldl' f Empty mat.rows }
--   where
--     k :: MatchRow -> Seq (Pattern l) -> MatchRow
--     k row =
--       let g row i p = handlePattern (conVars `Seq.index` i) p row id
--        in Seq.foldlWithIndex g (row & rColsL %~ Map.delete var)

--     g :: Pattern l -> MatchRow -> MatchRow
--     g (PAs _ p) row = g p row
--     g (PData _ args) row = k row args
--     g (POr ps _) row | Just args <- Map.lookup alt ps = k row args
--     g (POr _ (Just p)) row = g p row
--     g _ _ = error "unreachable"

--     f acc row = case (alt, Map.lookup var row.cols) of
--       -- add the row as is since it's wildcard
--       -- because it places no constraints on `var`
--       (_, Nothing) -> acc :|> row
--       (AltInt i, Just (PInt i')) | i == i' ->
--         -- delete the column since there are no sub patterns
--         acc :|> (row & rColsL %~ Map.delete var)
--       (AltString s, Just (PString s')) | s == s' ->
--         -- delete the column since there are no sub patterns
--         acc :|> (row & rColsL %~ Map.delete var)
--       -- otherwise replace the column with its sub patterns
--       (AltData con, Just (PData con' ps)) | con == con'->
--         let g row i p = handlePattern (conVars `Seq.index` i) p row id in
--           (acc :|>) $ Seq.foldlWithIndex
--             g (row & rColsL %~ Map.delete var) ps
--       -- since the constrcutors don't match delete the row
--       (_, Just p) | alt `Set.member` patternAltCons p ->
--         acc :|> g p row
--       _ -> acc

-- NOTE: partial Map.!, foldl1
-- columnPick :: PatternMatrix -> Elab (Var, TypeInfo)
-- columnPick (Matrix Empty) = error "empty match matrix"
-- columnPick (Matrix rows@(row :<| _)) =
--   let (k, _) = List.foldl1 findMax . Map.toList $
--         foldl' mkMap (row.cols $> (0 :: Int)) rows in
--   fmap (k,) . patternTypeInfo $ row.cols Map.! k
--   where
--     findMax a b
--       | snd b > snd a = b
--       | otherwise     = a
--     mkMap acc (Row m _ _ _) =
--       Map.foldlWithKey' (\acc k _ -> Map.insertWith (+) k 1 acc) acc m

matchComplete :: TypeInfo -> Set DataId -> Bool
matchComplete (TypeInfo{span}) cons = Just (length cons) == span

mapFoldlM :: (acc -> k -> v -> Elab acc) -> acc -> Map k v -> Elab acc
mapFoldlM f z0 xs = Map.foldrWithKey c return xs z0
  where c x y k z = f z x y >>= k; {-# INLINE c #-}

matchCompile :: PatternMatrix -> Elab Term
matchCompile = matchCompile
-- matchCompile (Matrix Empty) = do
--   pure $ TError "in-exhaustive match"
-- matchCompile (Matrix (Row col Nothing binds body :<| _))
--   | null col = pure $ Leaf (k binds body)
-- matchCompile k (Matrix (row@(Row col (Just (v, p)) _ _) :<| rs) pick)
--   | null col =
--     let row' = rowMatchVar v p row { guard = Nothing } in
--       matchCompile k (Matrix (row' :<| rs) pick)
-- matchCompile k mat@(Matrix rows pick) = do
--   -- pick a variable to scrutinize
--   let (var, tinfo) = pick mat
--   -- collect the heads of constructors tested against var
--   -- then compile the subtrees and combine them into a switch
--   (def, cons, cases) <- foldM (goRow var) (Nothing, mempty, Empty) rows
--   case def of
--     Just {} ->
--       pure $ Switch var cases def
--     Nothing | matchComplete tinfo cons ->
--       pure $ Switch var cases Nothing
--     Nothing ->
--       Switch var cases . Just
--         <$> defaultCase var
--   where
--     defaultCase v = matchCompile k (rowDefault v mat)

--     -- filter out repeated test against constructors
--     specialize v acc@(d, cs, ms) c = do
--       if Set.notMember c cs then do
--         xs <- matchConVars c
--         let body = rowSpecialize v c xs mat
--         m <- Alt c xs <$> shift (length xs) (matchCompile k body)
--         pure (d, Set.insert c cs, ms :|> m)
--       else pure acc

--     -- for each constructor, specialize the pattern matrix
--     goHead _ acc@(Just {}, _, _) _ = pure acc
--     goHead v acc (PData c _)     = specialize v acc c
--     goHead v acc (POr qs p)      = do
--       acc <- mapFoldlM (\acc c args -> goHead v acc (PData c args)) acc qs
--       -- check if we hit a wildcard while compiling subpatterns
--       case maybe Nothing (fst . lower) p of
--         -- compile a default case
--         Nothing -> defaultCase v <&> \d ->
--           acc & _1 ?~ d
--         -- compile the generalized pattern
--         Just q -> goHead v acc q

--     goRow _  acc@(Just {}, _, _) _ = pure acc
--     goRow v acc r =
--       let p     = Map.lookup v r.cols
--           pCons = maybe Set.empty patternDataCons p
--           acc'  = maybe (pure acc) (goHead v acc) p
--         -- don't emit matches where guard and `Alt` contradict
--         in case r.guard of
--           Just (v', p') | v == v'
--                         , Set.disjoint pCons (patternDataCons p') ->
--             pure acc
--           Just _ ->
--             acc' <&> _2 .~ (acc ^. _2)
--           Nothing -> acc'
