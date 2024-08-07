{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}

module Lang where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Zip
import Data.Foldable
import Data.Functor
import Data.IntMap                (IntMap)
import Data.IntMap.Strict         qualified as IntMap
import Data.IORef
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Maybe                 (isJust)
import Data.Sequence              (Seq (..))
import Data.Sequence              qualified as Seq
import Data.Set                   (Set)
import Data.Set                   qualified as Set
import Data.String                (IsString (..))
import Data.Text                  qualified as Text
import Data.Text.Short            (ShortText)
import Data.Text.Short            qualified as TextS
import Data.Typeable              (Typeable)
import GHC.Exts                   (oneShot)
import GHC.IO.Unsafe              (unsafePerformIO)
import Lens.Micro.GHC
import Lens.Micro.Internal
import Witherable

--- UTILITIES

newtype TrivialEq r
  = TrivialEq r
  deriving (Show)
    via r
  deriving (Functor)

instance Eq (TrivialEq r) where
  (==) _ _ = True

instance Ord (TrivialEq r) where
  compare _ _ = EQ

newtype TrivialOrd r
  = TrivialOrd r
  deriving (Show)
    via r
  deriving (Eq, Functor)

instance (Eq r) => Ord (TrivialOrd r) where
  compare _ _ = EQ

newtype TrivialShow r
  = TrivialShow r
  deriving (Eq, Ord)
    via r

instance Show (TrivialShow r) where
  show = const "<omitted>"

newtype RecFold a b
  = MkRecFold { runRecFold :: a -> (RecFold a b -> b) -> b }

pattern RecFold :: (a -> (RecFold a b -> b) -> b) -> RecFold a b
pattern RecFold f <- MkRecFold f
  where RecFold f = MkRecFold (oneShot f)

foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2 c i xs =
  foldr f (const i) xs . RecFold . foldr g (\_ _ -> i) where
    g e2 r2 e1 r1 = c e1 e2 (r1 (RecFold r2))
    f e r (MkRecFold x) = x e r
{-# INLINEABLE foldr2 #-}

todo :: HasCallStack => a
todo = error "TODO"

type StrictText = Text.Text

--- VARIABLES

data NameKind where
  -- deBruijn indicies
  BoundName :: NameKind
  -- unique within a module
  LocalName :: NameKind
  -- globally unique
  GlobalName :: NameKind
  deriving (Eq)

data Name = Name
  { kind    :: {-# UNPACK #-} !(TrivialOrd NameKind)
  , display :: {-# UNPACK #-} !(TrivialEq ShortText)
  , id      :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord)

pattern Bound, Local, Global :: ShortText -> Int -> Name
pattern Bound x i = Name (TrivialOrd BoundName) (TrivialEq x) i
pattern Local x i = Name (TrivialOrd LocalName) (TrivialEq x) i
pattern Global x i = Name (TrivialOrd GlobalName) (TrivialEq x) i
{-# COMPLETE Local, Global, Bound #-}

instance Show Name where
  show (Bound x  i)  = "%" <> TextS.toString x <> "." <> show i
  show (Local x  i)  = "$" <> TextS.toString x <> "." <> show i
  show (Global x  i) = "@" <> TextS.toString x <> "." <> show i

nameSource :: IORef Int
nameSource = unsafePerformIO (newIORef 0)
{-# NOINLINE nameSource #-}

freshLocalName :: MonadIO m => ShortText -> m Name
freshLocalName x =
  liftIO $ atomicModifyIORef' nameSource \i ->
    let !z = i + 1 in (z, Local x z)

freshGlobalName :: MonadIO m => ShortText -> m Name
freshGlobalName x =
  liftIO $ atomicModifyIORef' nameSource \i ->
    let !z = i + 1 in (z, Global x z)

newtype NameMap x
  = NameMap { nameMap :: IntMap x }
  deriving (Foldable, Functor, Monoid, Semigroup, Traversable)
  deriving (Show)
    via (IntMap x)

type instance Index (NameMap a) = Name
type instance IxValue (NameMap a) = a

instance Ixed (NameMap m) where
  ix k f (NameMap m) = case IntMap.lookup k.id m of
     Just v  -> f v <&> NameMap . flip (IntMap.insert k.id) m
     Nothing -> pure (NameMap m)
  {-# INLINE ix #-}

--- PRIMITIVE TYPES

data Literal where
  LInt :: Integer -> Literal
  LChar :: Char -> Literal
  LString :: StrictText -> Literal
  deriving (Eq, Ord, Show)

--- RAW AST

type RawName = ShortText

data RawType where
  RTyCon :: RawName -> RawType
  RTyFun :: RawType -> RawType -> RawType
  deriving (Eq, Ord, Show)

rawTypeSplit :: RawType -> (Seq RawType, RawType)
rawTypeSplit = go Empty
  where
    go acc (RTyFun a b) = do
      go (acc :|> a) b
    go acc t = (acc, t)

-- data PatternKind
--   = Low
--   | High

-- data RawConstructor where
--   RCLit :: Literal -> RawConstructor
--   RCData :: RawName -> RawConstructor
--   deriving (Eq, Ord, Show)

-- data Pattern (l :: PatternKind) where
--   PAnnot :: Pattern l -> RawType -> Pattern l
--   PAs :: Pattern 'High -> RawName -> Pattern 'High
--   PCon :: RawConstructor -> Seq (Pattern 'High) -> Pattern l
--   POr :: Seq (Pattern 'Low) -> Pattern 'High -> Pattern l
--   PWild :: Pattern 'High

-- deriving instance Show (Pattern l)
-- deriving instance Eq (Pattern l)
-- -- deriving instance Ord (Pattern l)

-- pattern PVar :: RawName -> Pattern 'High
-- pattern PVar x = PAs PWild x

-- lowerPattern :: Pattern 'High -> (Maybe (Pattern 'Low), Maybe RawName)
-- lowerPattern (PAnnot p t) =
--   let (p', x) = lowerPattern p in
--     (flip PAnnot t <$> p', x)
-- lowerPattern (PAs p x)   = lowerPattern p $>  Just x
-- lowerPattern (PCon c ps) = (Just (PCon c ps), Nothing)
-- lowerPattern (POr qs p)  = (Just (POr qs p), Nothing)
-- lowerPattern PWild       = (Nothing, Nothing)

data RawTermDef where
  RTDAnno :: RawName -> RawType -> RawTermDef
  RTDTerm :: RawName -> Raw -> RawTermDef
  deriving (Eq, Show)

data RawPattern where
  RPAnnot :: RawPattern -> RawType -> RawPattern
  RPAs :: RawPattern -> RawName -> RawPattern
  RPData :: RawName -> [RawPattern] -> RawPattern
  RPLit :: Literal -> RawPattern
  RPOr :: RawPattern -> RawPattern -> RawPattern
  RPWild :: RawPattern
  RPVar :: RawName -> RawPattern
  deriving (Eq, Show)

data Raw where
  RAnnot :: Raw -> RawType -> Raw
  RApp :: Raw -> [Raw] -> Raw
  RCase :: Raw -> [(RawPattern, Raw)] -> Raw
  RData :: RawName -> [Raw] -> Raw
  RLambda :: [RawName] -> Raw -> Raw
  RLet :: [RawTermDef] -> Raw -> Raw
  RLetRec :: [RawTermDef] -> Raw -> Raw
  RLit :: Literal -> Raw
  RPrim :: RawName -> [Raw] -> Raw
  RVar :: RawName -> Raw
  deriving (Eq, Show)

data RawDef where
  RDPrim :: RawName -> RawType -> RawDef
  RDTerm :: RawName -> RawType -> Raw -> RawDef
  RDData :: RawName -> [(RawName, [RawType], RawType)] -> RawDef
  deriving (Eq, Show)

--- LOWERED AST

data TyMeta where
  MUnsolved :: Int -> TyMeta
  MSolved :: Type -> TyMeta
  deriving (Eq)

type TyCon = Name

data Type where
  -- TyPrim :: PrimTy -> Type
  TyCon :: TyCon -> Type
  TyMeta' :: TrivialShow (IORef TyMeta) -> Type
  TyFun :: Seq Type -> Type -> Type
  TyNeg :: Seq Type -> Type
  deriving (Eq, Show)

pattern TyMeta :: IORef TyMeta -> Type
pattern TyMeta t = TyMeta' (TrivialShow t)

typeSplit :: Type -> ([Type], Type)
typeSplit (TyFun xs t) = (toList xs, t)
typeSplit t            = ([], t)

data Var = MkVar Name (TrivialEq Type)
  deriving (Eq, Ord, Show)

varType :: Var -> Type
varType (Var _ t) = t

pattern Var :: Name -> Type -> Var
pattern Var n t = MkVar n (TrivialEq t)
{-# COMPLETE Var :: Var #-}

type Label = Var
pattern Label :: Name -> Seq Type -> Label
pattern Label n t <- Var n (TyNeg t)
  where Label n t = Var n (TyNeg t)
{-# COMPLETE Label #-}

type PrimOp = Name

data DataTag = DataTag
  { name :: Name
  , tag  :: TrivialOrd Int
  }
  deriving (Eq, Ord, Show)

data Value where
  VLit :: Literal -> Value
  VData :: DataTag -> Seq Var -> Value
  -- VClos ::
  deriving (Show)

data Constructor
  = CData DataTag
  | CLit Literal
  deriving (Show)

data Alt = Alt Constructor (Seq Var) Term
  deriving (Show)

data Term where
  TAbsurd :: Maybe ShortText -> Term
  TApp :: Var -> Label -> Seq Var -> Term
  TCase :: Var -> Seq Alt -> Maybe Term -> Term
  TJmp :: Label -> Seq Var -> Term
  TLetF :: Seq (Var, Label, Seq Var, Term) -> Term -> Term
  TLetK :: Seq (Label, Seq Var, Term) -> Term -> Term
  TLetP :: Var -> PrimOp -> Seq Var -> Term -> Term
  TLetV :: Var -> Value -> Term -> Term
  deriving (Show)

--- ELABORATION

data ElabErrorMsg
  = ErrTypeUnkown RawType
  | ErrTypeUnify Type Type
  | ErrVarUnknown RawName
  | ErrConUnknown RawName
  | ErrConArity RawName Int Int
  | ErrPrimUnknown RawName
  | ErrPrimArity RawName Int Int
  | ErrInvalidAnnotation RawName
  | ErrDuplicate String RawName
  | ErrOther String
  deriving (Show)

instance IsString ElabErrorMsg where
  fromString = ErrOther

newtype ElabError
  = ElabError { msg :: ElabErrorMsg }
  deriving (Show, Typeable)

instance Exception ElabError where
  displayException p = show p.msg

elabError ::(HasCallStack, MonadIO m) => ElabErrorMsg -> m a
elabError = liftIO . throwIO . ElabError

metaSource :: IORef Int
metaSource = unsafePerformIO (newIORef 0)
{-# NOINLINE metaSource #-}

freshMeta :: MonadIO m => m Type
freshMeta = liftIO do
  m <- atomicModifyIORef' nameSource \i ->
    let !z = i + 1 in (z, MUnsolved z)
  TyMeta <$> newIORef m

tyForce :: MonadIO m => Type -> m Type
tyForce (TyMeta m) = liftIO $ go m
  where
    go m = readIORef m >>= \case
      MSolved (TyMeta m') -> do
        m' <- go m'
        writeIORef m (MSolved m')
        pure m'
      MSolved m -> pure m
      _ -> pure (TyMeta m)
tyForce t = pure t

tyZonk :: MonadIO m => Type -> m Type
tyZonk t = tyForce t >>= \case
  TyFun as b  -> TyFun <$> traverse tyZonk as <*> tyZonk b
  TyNeg t     -> TyNeg <$> traverse tyZonk t
  x           -> pure x

tySolve :: (HasCallStack, MonadIO m) => IORef TyMeta -> Type -> m ()
tySolve m t = liftIO $
  readIORef m >>= \case
    MUnsolved _ -> tyForce t >>= go
    _ -> error "solved meta passed to `tySolve`"
  where
    go (TyFun as b) = traverse_ go as >> go b
    go (TyNeg t)    = traverse_ go t
    go _            = undefined

tyUnify :: (HasCallStack, MonadIO m) => Type -> Type -> m ()
tyUnify x y = do
  x <- tyForce x
  y <- tyForce y
  go x y
  where
    go x y | x == y = pure ()
    go (TyMeta m) t = tySolve m t
    go t (TyMeta m) = tySolve m t
    go (TyFun as b) (TyFun as' b') | length as == length as' =
      sequence_ (mzipWith go as as') >> go b b'
    go (TyNeg t) (TyNeg t') | length t == length t' =
      sequence_ (mzipWith go t t')
    go x y = do
      x <- tyZonk x
      y <- tyZonk y
      elabError (ErrTypeUnify x y)

data ElabT = Elab
  { ctx   :: Context
  , level :: Int
  }

type Elab r = ReaderT ElabT IO r

ctxBindName :: Context -> RawName -> Var -> Context
ctxBindName Context{..} x v = Context
  { rawTerms = rawTerms & (at x ?~ v), ..}

ctxBindNames :: (MonadZip m, Foldable m) => Context -> m RawName -> m Var -> Context
ctxBindNames ctx xs vs = foldl' (\ctx (x, v) -> ctxBindName ctx x v) ctx (mzip xs vs)

elabContext :: Elab Context
elabContext = asks (.ctx)

elabShiftLevel1 :: (Int -> Elab r) -> Elab r
elabShiftLevel1 = elabShiftLevel 1

elabShiftLevel :: Int -> (Int -> Elab r) -> Elab r
elabShiftLevel by k = ReaderT \Elab{..} ->
  runReaderT (k level) Elab { level = level + by, ..}

elabSetLevel :: Int -> Elab r -> Elab r
elabSetLevel l k = ReaderT \Elab{..} ->
  runReaderT k Elab { level = l, ..}

elabBindName :: RawName -> Var -> Elab r -> Elab r
elabBindName x v k = local (\Elab{..} -> Elab{ctx = ctxBindName ctx x v, ..}) k

elabBindNames ::(MonadZip m, Foldable m) => m RawName -> m Var -> Elab r -> Elab r
elabBindNames xs vs k = local (\Elab{..} -> Elab{ctx = ctxBindNames ctx xs vs, ..}) k

elabTypeRaw :: (HasCallStack, MonadIO m) => Map RawName Name -> RawType -> m Type
elabTypeRaw ctx t = go ctx Empty t
  where
    go ctx acc (RTyFun a b) = do
      a' <- go ctx Empty a
      go ctx (acc :|> a') b
    go ctx acc r@(RTyCon c) =
      case ctx ^. at c of
        Nothing            -> elabError (ErrTypeUnkown r)
        Just t  | null acc -> pure $ TyCon t
        Just t             -> pure $ TyFun acc (TyCon t)

elabType :: HasCallStack => RawType -> Elab Type
elabType t = do
  ctx <- elabContext
  elabTypeRaw ctx.rawTypeCons t

freshLocal :: MonadIO m => RawName -> Type -> m Var
freshLocal x t = flip Var t <$> freshLocalName x

freshGlobal :: MonadIO m => RawName -> Type -> m Var
freshGlobal x t = flip Var t <$> freshGlobalName x

freshLabel :: MonadIO m => RawName -> Seq Type -> m Label
freshLabel x ts = flip Label ts <$> freshLocalName x

data Cont where
  Abstract :: Label -> Cont
  Wrap :: (Var -> Elab Term) -> Cont

applyCont :: HasCallStack => Cont -> Var -> Elab Term
applyCont (Abstract k) x = pure $ TJmp k (pure x)
applyCont (Wrap k) x     = k x

{-
freshBound :: CPS.Type s -> Cont s -> Infer s (CPS.Var s, CPS.Term s)
freshBound t k = do
  Context{..} <- askCtx
  let v = CPS.Var (Bound "x" termLevel) t
  case k of
    Abstract k ->
      pure (v, CPS.TJmp k [v])
    Wrap k ->
      localCtx (\ctx -> ctx
        { termLevel = termLevel + 1
        }) (k v <&> _1 .~ v)
-}

lookupRaw :: HasCallStack => RawName -> Elab Var
lookupRaw x = do
  Context{..} <- elabContext
  case rawTerms ^. at x of
    Nothing -> elabError (ErrVarUnknown x)
    Just v  -> pure v

litType :: HasCallStack => Literal -> Elab Type
litType (LInt _)    = do
  Context{..} <- elabContext
  case rawTypeCons ^. at "Int#" of
    Nothing -> elabError "Int# not defined"
    Just t  -> pure (TyCon t)
litType (LChar _)   = do
  Context{..} <- elabContext
  case rawTypeCons ^. at "Char#" of
    Nothing -> elabError "Char# not defined"
    Just t  -> pure (TyCon t)
litType (LString _) = do
  Context{..} <- elabContext
  case rawTypeCons ^. at "String#" of
    Nothing -> elabError "String# not defined"
    Just t  -> pure (TyCon t)

elabTerms
  :: (HasCallStack, Foldable f)
  => f Raw -> f Type -> (Seq Var -> Elab Term)
  -> Elab Term
elabTerms xs ts k = foldr2 c k xs ts Empty
  where
    {-# INLINE c #-}
    c x t k zs = elabTerm x t (Wrap \z -> k (zs :|> z))

elabTerm :: HasCallStack => Raw -> Type -> Cont -> Elab Term
elabTerm (RAnnot x t') t k = do
  elabType t' >>= tyUnify t
  elabTerm x t k
elabTerm (RApp f (Seq.fromList -> xs)) t k = do
  ts <- mapM (const freshMeta) xs
  elabTerm f (TyFun ts t) $ Wrap \v -> elabTerms xs ts \vs ->
    case k of
      Abstract k -> do
        tyUnify (varType k) (TyNeg ts)
        pure $ TApp v k vs
      Wrap k -> elabShiftLevel1 \i -> do
        let x = Var (Bound "x" i) t
        kx <- k x
        k <- freshLabel "k" [t]
        pure $ TLetK [(k, [x], kx)] (TApp v k vs)
elabTerm (RCase _x _es) _t _k = do
  -- t' <- freshMeta
  -- elab x t $ Wrap \v ->
  --   case k of
  --     Abstract k -> todo
  --     Wrap k -> todo
  -- where
  --   mkRow k (p, e) = _
  todo
elabTerm (RData rc xs) t k = do
  Context{..} <- elabContext
  case rawDataCons ^. at rc of
    Nothing           -> elabError (ErrConUnknown rc)
    Just (tag, ts, t') -> do
      unless (length ts == length xs) $
        elabError (ErrConArity rc (length ts) (length xs))
      elabTerms xs ts \vs -> do
        tyUnify t t'
        x <- freshLocal "d" t'
        e <- applyCont k x
        pure $ TLetV x (VData tag vs) e
elabTerm (RLambda (Seq.fromList -> xs) e) t k = do
  f <- freshLocal "f" t
  j <- elabShiftLevel1 \ik -> elabShiftLevel (length xs) \ixs -> do
    t' <- freshMeta
    let k = Label (Bound "k" ik) [t']
    vs <- Seq.traverseWithIndex
      (\i x -> Var (Bound x (ixs + i)) <$> freshMeta) xs
    e <- elabBindNames xs vs $
      elabTerm e t' (Abstract k)
    tyUnify t (TyFun (varType <$> vs) t')
    pure $ TLetF [(f, k, vs, e)]
  j <$> applyCont k f
-- elab (RLet False xs e) t k = do
--   elabCtx <- ask
--   -- TODO: we need to modify the local env somehow for annotations
--   foldM `flip` (elabCtx) `flip` xs $ \cases
--     acc (RTDAnno x t) -> do
--       t <- elabType t
--       v <- freshLocal x t
--       pure (acc { ctx = ctxBindName acc.ctx x v })
--     acc (RTDTerm x e) -> do
--       case acc.ctx.rawTerms ^. at x of
--         Just v  -> pure $ Just (v, e)
--         Nothing -> do
--           t <- freshMeta
--           Just . (,e) <$> freshLocal x t
--   todo
elabTerm (RLetRec _ _) _t _k = todo
elabTerm (RLet xs _e) _t _k = do
  -- TODO: we need to modify the local env somehow for annotations
  _xs <- flip witherM xs \case
    RTDAnno x t -> do
      t <- elabType t
      freshLocal x t $> Nothing
    RTDTerm x e -> do
      Context{..} <- elabContext
      case rawTerms ^. at x of
        Just v  -> pure $ Just (v, e)
        Nothing -> do
          t <- freshMeta
          Just . (,e) <$> freshLocal x t
  -- if rec then do
  --   -- since we don't have recursive values, this means that all the variables
  --   -- must be functions
  --   -- fs <- forM xs undefined
  --   TLetF todo <$> elab e t k
  -- else do
  --   -- since this isn't a block of recursive functions, it is a block of values
  --   -- that may include functions, so we split them up into a group of funcions
  --   -- and a group of values
  todo
elabTerm (RLit l) t k = do
  litType l >>= tyUnify t
  x <- freshLocal "l" t
  e <- applyCont k x
  pure $ TLetV x (VLit l) e
elabTerm (RPrim rp xs) t k = do
  Context{..} <- elabContext
  case rawPrims ^. at rp of
    Nothing          -> elabError (ErrPrimUnknown rp)
    Just (p, ts, t') -> do
      unless (length ts == length xs) $
        elabError (ErrPrimArity rp (length ts) (length xs))
      elabTerms xs ts \vs -> do
        tyUnify t t'
        x <- freshLocal "p" t'
        e <- applyCont k x
        pure $ TLetP x p vs e
elabTerm (RVar x) t k = do
  Context{..} <- elabContext
  case rawTerms ^. at x of
    Nothing -> elabError (ErrVarUnknown x)
    Just v  -> do
      tyUnify t (varType v)
      applyCont k v

-- data MatchRow = Row
--   { cols :: Map Var (Pattern 'Low)
--   , vars :: Map RawName Var
--   , body :: Label
--   }

-- rColsL :: Var -> Lens' MatchRow (Maybe (Pattern 'Low))
-- rColsL v f r = (\cs -> r {cols = cs}) <$> Map.alterF f v r.cols

-- rVarsL :: String -> Lens' MatchRow (Maybe Var)
-- rVarsL v f r = (\vs -> r {vars = vs}) <$> Map.alterF f v r.vars

-- rowMatchVar :: MatchRow -> Var -> Pattern 'Low -> MatchRow
-- rowMatchVar r v p = r & rColsL v ?~ p

-- rowBindVar :: MatchRow -> Var -> String -> MatchRow
-- rowBindVar r v x = r & rVarsL x ?~ v

-- newtype MatchMatrix
--   = Matrix (Seq MatchRow)

-- -- convert a `Pattern 'High` to `Pattern 'Low` making sure to
-- -- add any names introduced by as patterns to the row
-- handlePattern :: Var -> Pattern 'High -> MatchRow -> MatchRow
-- handlePattern v p r = do
--   maybe
--     (r' & rColsL v .~ Nothing)
--     (rowMatchVar r' v) q
--   where
--     (q, b) = lowerPattern p
--     r'     = maybe r (rowBindVar r v) b

-- rowSpecialize :: Var -> RawConstructor -> Seq Var -> MatchMatrix -> MatchMatrix
-- rowSpecialize v c vs (Matrix rs) = Matrix $ foldMap' f rs
--   where
--     f r = case r ^. rColsL v of
--       -- add the row as is since it's wildcard
--       Nothing -> pure r
--       -- expand the or pattern into multiple rows
--       Just (POr qs p) ->
--         f (handlePattern v p r) <> foldMap (f . rowMatchVar r v) qs
--       Just (PCon c' ps) | c == c' -> do
--         -- otherwise replace the column with its sub patterns
--         let g row i p = handlePattern (vs `Seq.index` i) p row
--         pure $ Seq.foldlWithIndex g (r & rColsL v .~ Nothing) ps
--       -- since the constrcutors don't match delete the row
--       _ -> Empty

-- rowDefault :: Var -> MatchMatrix -> MatchMatrix
-- rowDefault v (Matrix rs) = Matrix $ foldMap' f rs
--   where
--     f r = case r ^. rColsL v of
--       -- add the row as is
--       Nothing -> Seq.singleton r
--       -- expand the or pattern into multiple rows
--       Just (POr qs p) ->
--         f (handlePattern v p r) <> foldMap (f . rowMatchVar r v) qs
--       -- delete rows with constructor patterns
--       Just _ -> Empty

-- elabMatch :: MatchMatrix -> Elab Term
-- elabMatch (Matrix Empty) = pure . TAbsurd $ Just "in-exhaustive match"
-- elabMatch (Matrix (Row col xs k :<| _)) | null col = pure $ TJmp k undefined
-- elabMatch m@(Matrix rows) = do
--   let (var, tinfo) = undefined m
--   -- collect the heads of constructors tested against var
--   -- then compile the subtrees and combine them into a switch
--   (def, cons, cases) <- foldM (goRow var) (Nothing, mempty, Empty) rows

--   todo
--   where
--     defaultCase v = elabMatch (rowDefault v m)

--     goHead _ acc@(Just {}, _, _) _ = pure acc
--     goHead v acc@(d, cs, ms) (PCon c _) =
--       if Set.notMember c cs then do
--         -- xs <- matchConVars c
--         -- let body = rowSpecialize v c xs m
--         -- m <- Alt c xs <$> shift (length xs) (matchCompile k body)
--         -- pure (d, Set.insert c cs, ms :|> m)
--         todo
--       else pure acc
--     goHead v acc (POr qs p)      = do
--       acc <- foldM (goHead v) acc qs
--       -- check if we hit a wildcard while compiling subpatterns
--       case fst $ lowerPattern p of
--         -- compile a default case
--         Nothing -> defaultCase v <&> \d ->
--           acc & _1 ?~ d
--         -- compile the generalized pattern
--         Just q -> goHead v acc q

--     goRow _  acc@(Just {}, _, _) _ = pure acc
--     goRow v acc r = do
--       case Map.lookup v r.cols of
--         Just q -> goHead v acc q
--         _      -> pure acc

data Context = Context
  { rawTerms    :: Map RawName Var
  , rawPrims    :: Map RawName (PrimOp, [Type], Type)
  , rawDataCons :: Map RawName (DataTag, [Type], Type)
  , rawTypeCons :: Map RawName Name
  }
  deriving (Show)

data RawContext = RawContext
  { dataCons :: Map RawName (DataTag, [RawType], Name, RawType)
  , typeCons :: Map RawName (Name, Set DataTag)
  , terms    :: Map RawName (Name, RawType, Raw)
  , prims    :: Map RawName (PrimOp, RawType)
  }
  deriving (Show)

data RawModule = RawModule
  { dataCons :: Map RawName (DataTag, [Type], Type)
  , prims    :: Map RawName (PrimOp, [Type], Type)
  , terms    :: Map RawName Var
  , typeCons :: Map RawName (Name, Set DataTag)
  }
  deriving (Show)

data Module = Module
  { dataCons :: NameMap (Int, Type)
  , prims    :: NameMap Type
  , terms    :: NameMap (Term, Type)
  , typeCons :: NameMap (Set Name)
  }

rawContext :: (HasCallStack, MonadIO m) => [RawDef] -> m RawContext
rawContext = foldM fDef (RawContext
  { dataCons = mempty
  , typeCons = mempty
  , terms    = mempty
  , prims    = mempty
  })
  where
    checkDup thing m x = do
      when (isJust $ m ^. at x) $ do
        elabError (ErrDuplicate thing x)
      freshLocalName x

    fCon (conSet, dataCons) (raw, args, ret) = do
      name <- checkDup "data constructor" dataCons raw
      let
        tag = DataTag name (TrivialOrd $ length conSet)
        conSet' = Set.insert tag conSet
        result  = (tag, args, name, ret)
      pure (conSet', dataCons & at raw ?~ result)

    fDef RawContext{..} (RDData raw xs) = do
      name <- checkDup "type constructor" typeCons raw
      (conSet, dataCons') <- foldM fCon (mempty, dataCons) xs
      pure RawContext
        { typeCons = typeCons & at raw ?~ (name, conSet)
        , dataCons = dataCons'
        , ..
        }
    fDef RawContext{..} (RDPrim raw t) = do
      name <- checkDup "primitive function" prims raw
      pure RawContext{ prims = prims & at raw ?~ (name, t), .. }
    fDef RawContext{..} (RDTerm raw t e) = do
      name <- checkDup "top-level" terms raw
      pure RawContext{ terms = terms & at raw ?~ (name, t, e), .. }

genContext1 :: (HasCallStack, MonadIO m) => [RawDef] -> m (RawModule)
genContext1 xs = do
  typeCons <- foldM `flip` mempty `flip` xs $ \cases
    typeCons (RDData raw _) -> do
      name <- checkDup "type constructor" typeCons raw
      pure $ typeCons & at raw ?~ name
    typeCons _ -> pure typeCons

  foldM (fDef typeCons) `flip` xs $ RawModule
      { dataCons = mempty
      , prims    = mempty
      , terms    = mempty
      , typeCons = mempty
      }

  where
    checkDup thing m x = do
      when (isJust $ m ^. at x) $ do
        elabError (ErrDuplicate thing x)
      freshLocalName x

    fCon tCon tParent (conSet, dataCons) (raw, rArgs, rRet) = do
      name <- checkDup "data constructor" dataCons raw
      args <- mapM (elabTypeRaw tCon) rArgs
      ret <- elabTypeRaw tCon rRet
      unless (ret == tParent) $
        elabError "bad data constructor"
      let
        tag = DataTag name (TrivialOrd $ length conSet)
        conSet' = Set.insert tag conSet
        result  = (tag, args, ret)
      pure (conSet', dataCons & at raw ?~ result)

    fDef tCon RawModule{..} (RDData raw xs) = do
      let name = tCon Map.! raw
      (conSet, dataCons) <- foldM
        (fCon tCon (TyCon name))
        (mempty, dataCons)
        xs
      pure RawModule
        { typeCons = typeCons & at raw ?~ (name, conSet)
        , dataCons
        , ..
        }
    fDef tCons RawModule{..} (RDPrim raw t) = do
      name <- checkDup "primitive function" prims raw
      (args, ret) <- typeSplit <$> elabTypeRaw tCons t
      pure RawModule{ prims = prims & at raw ?~ (name, args, ret), .. }
    fDef tCons RawModule{..} (RDTerm raw t _) = do
      name <- checkDup "top-level" terms raw
      var <- Var name <$> elabTypeRaw tCons t
      pure RawModule{ terms = terms & at raw ?~ var, .. }

genContext :: (HasCallStack, MonadIO m) => RawContext -> m ([(Var, Raw)], RawModule)
genContext raw = do
  let
    typeCons = fst <$> raw.typeCons
    elabType = elabTypeRaw typeCons
  rawTerms <- forM raw.terms \(name, t, e) ->
    (, e) . Var name <$> elabType t
  prims <- forM raw.prims \(name, t) -> do
    (args, ret) <- typeSplit <$> elabType t
    pure (name, args, ret)
  dataCons <- forM raw.dataCons \(tag, rArgs, nParent, rRet) -> do
    args <- mapM elabType rArgs
    ret <- elabType rRet
    unless (ret == TyCon nParent) $
      elabError "bad data constructor"
    pure (tag, args, ret)

  pure ([], RawModule
    { terms = fst <$> rawTerms
    , prims
    , dataCons
    , typeCons = raw.typeCons
    })
  where
    _nameMap = NameMap . IntMap.fromAscList
    -- init = Context
    --   { rawTerms    = mempty
    --   , rawPrims    = mempty
    --   , rawDataCons = mempty
    --   , rawTypeCons = fst <$> ctx.typeCons
    --   }


-- rawContext' :: HasCallStack => [RawDef] -> IO Context
-- rawContext' = foldM fDef (Context
--   { rawTerms = mempty
--   , rawPrims = mempty
--   , rawDataCons = mempty
--   , rawTypeCons = mempty
--   })
--   where
--     fnSplit (TyFun xs t) = (toList xs, t)
--     fnSplit t            = ([], t)

--     checkDup thing m x = do
--       when (isJust $ m ^. at x) $ do
--         elabError (ErrDuplicate thing x)
--       freshLocalName x

--     fCon tyCons tParent (conSet, dataCons) (raw, args, ret) = do
--       name <- checkDup "data constructor" dataCons raw
--       let
--         conSet' = Set.insert name conSet
--         index   = Set.findIndex name conSet'
--       ret <- elabTypeRaw tyCons ret
--       unless (ret == tParent) $
--         elabError "bad data constructor"
--       args <- mapM (elabTypeRaw tyCons) args
--       pure
--         ( conSet'
--         , dataCons & at raw
--                 ?~ (DataTag name index, args, ret)
--         )
--     fDef Context{..} (RDData raw (xs)) = do
--       name <- checkDup "type constructor" rawTypeCons raw
--       let tyCons = rawTypeCons & at raw ?~ name
--       (_, rawDataCons) <- foldM (fCon tyCons (TyCon name)) (mempty, rawDataCons) xs
--       pure Context
--         { rawDataCons, rawTypeCons = tyCons, .. }
--     fDef ctx@Context{..} (RDPrim raw t) = do
--       name <- checkDup "primitive function" rawPrims raw
--       (ts, t) <- fnSplit <$> elabTypeRaw ctx.rawTypeCons t
--       pure Context{ rawPrims = rawPrims & at raw ?~ (name, ts, t), .. }
--     fDef ctx@Context{..} (RDTerm raw t _) = do
--       name <- checkDup "top-level" rawTerms raw
--       var <- Var name <$> elabTypeRaw ctx.rawTypeCons t
--       pure Context{ rawTerms = rawTerms & at raw ?~ var, .. }

