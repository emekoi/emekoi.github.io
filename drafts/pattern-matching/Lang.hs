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
import Data.List                  qualified as List
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
import Data.Traversable
import Data.Typeable              (Typeable)
import GHC.Exts                   (oneShot)
import GHC.IO.Unsafe              (unsafePerformIO)
import Lens.Micro.GHC
import Lens.Micro.Internal
-- import Witherable

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
  where
    RecFold f = MkRecFold (oneShot f)

foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2 c i xs =
  foldr f (const i) xs . RecFold . foldr g (\_ _ -> i)
  where
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
  show (Bound x i)  = "%" <> TextS.toString x <> "." <> show i
  show (Local x i)  = "$" <> TextS.toString x <> "." <> show i
  show (Global x i) = "@" <> TextS.toString x <> "." <> show i

nameSource :: IORef Int
nameSource = unsafePerformIO (newIORef 0)
{-# NOINLINE nameSource #-}

freshLocalName :: (MonadIO m) => ShortText -> m Name
freshLocalName x =
  liftIO $ atomicModifyIORef' nameSource \i ->
    let !z = i + 1 in (z, Local x z)

freshGlobalName :: (MonadIO m) => ShortText -> m Name
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
  deriving (Eq, Ord)

instance Show Literal where
  show (LInt l)    = show l
  show (LChar c)   = show c
  show (LString s) = show s

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
  TyCon :: TyCon -> Type
  TyMeta :: IORef TyMeta -> Type
  TyFun :: Seq Type -> Type -> Type
  deriving (Eq)

instance Show Type where
  show (TyCon t) = show t
  show (TyFun args ret) =
    List.intercalate "->" (toList $ (show <$> args) :|> show ret)
  show (TyMeta _) = "?"

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

freshLocal :: (MonadIO m) => RawName -> Type -> m Var
freshLocal x t = flip Var t <$> freshLocalName x

freshGlobal :: (MonadIO m) => RawName -> Type -> m Var
freshGlobal x t = flip Var t <$> freshGlobalName x

data Label = MkLabel Name (TrivialEq (Seq Type))
  deriving (Eq, Ord, Show)

pattern Label :: Name -> Seq Type -> Label
pattern Label n t <- MkLabel n (TrivialEq t)
  where
    Label n t = MkLabel n (TrivialEq t)

{-# COMPLETE Label :: Label #-}

labelTypes :: Label -> Seq Type
labelTypes (Label _ ts) = ts

freshLabel :: (MonadIO m) => RawName -> Seq Type -> m Label
freshLabel x ts = flip Label ts <$> freshLocalName x

labelReturn :: Type -> Label
labelReturn t = Label (Bound "return" 0) [t]

type PrimOp = Name

data DataTag = DataTag
  { name :: Name
  , tag  :: TrivialOrd Int
  }
  deriving (Eq, Ord, Show)

data Value where
  VLit :: Literal -> Value
  VData :: DataTag -> Seq Var -> Value
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

dump :: Term -> String
dump = dumpTerm
  where
    dumpVar (Var x _) = show x
    dumpLabel (Label k _) = show k
    dumpValue (VLit l) = show l
    dumpValue (VData t xs) =
      "(" ++ unwords (show t.name : toList (dumpVar <$> xs)) ++ ")"
    dumpTerm (TAbsurd Nothing) = "absurd"
    dumpTerm (TAbsurd (Just msg)) = "(absurd " ++ show msg ++ ")"
    dumpTerm (TApp (dumpVar -> f) (dumpLabel -> k) xs) =
      "(" ++ unwords (f : k : toList (dumpVar <$> xs)) ++ ")"
    dumpTerm TCase{} = "TODO"
    dumpTerm (TJmp (dumpLabel -> k) xs) =
      "(jump " ++ unwords (k : toList (dumpVar <$> xs)) ++ ")"
    dumpTerm (TLetK xs e) =
      let
        f (k, xs, e) = "(label " ++ unwords
          [ dumpLabel k
          , "(" ++ unwords (toList $ dumpVar <$> xs) ++ ")"
          , dumpTerm e
          ] ++")"
      in "(" ++ unwords (toList (f <$> xs) ++[dumpTerm e]) ++ ")"
    dumpTerm (TLetF xs e) =
      let
        f (f, k, xs, e) = "(func " ++ unwords
          [ dumpVar f
          , dumpLabel k
          , "(" ++ unwords (toList $ dumpVar <$> xs) ++ ")"
          , dumpTerm e
          ] ++")"
      in "(" ++ unwords (toList (f <$> xs) ++[dumpTerm e]) ++ ")"
    dumpTerm (TLetP x p xs e) =
      let r = "(" ++ unwords (show p : toList (dumpVar <$> xs)) ++ ")" in
      "(value " ++ unwords [dumpVar x, r, dumpTerm e] ++ ")"
    dumpTerm (TLetV x v e) =
      "(value " ++ unwords [dumpVar x, dumpValue v, dumpTerm e] ++ ")"

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
  | ErrDefDuplicate String RawName
  | ErrDefOther String
  | ErrOther String
  deriving (Show)

instance IsString ElabErrorMsg where
  fromString = ErrOther

newtype ElabError
  = ElabError { msg :: ElabErrorMsg }
  deriving (Show, Typeable)

instance Exception ElabError where
  displayException p = show p.msg

elabError :: (HasCallStack, MonadIO m) => ElabErrorMsg -> m a
elabError = liftIO . throwIO . ElabError

metaSource :: IORef Int
metaSource = unsafePerformIO (newIORef 0)
{-# NOINLINE metaSource #-}

freshMeta :: (MonadIO m) => m Type
freshMeta = liftIO do
  m <- atomicModifyIORef' nameSource \i ->
    let !z = i + 1 in (z, MUnsolved z)
  TyMeta <$> newIORef m

tyForce :: (MonadIO m) => Type -> m Type
tyForce (TyMeta m) = liftIO $ go m
  where
    go m =
      readIORef m >>= \case
        MSolved (TyMeta m') -> do
          m' <- go m'
          writeIORef m (MSolved m')
          pure m'
        MSolved m -> pure m
        _ -> pure (TyMeta m)
tyForce t = pure t

tyZonk :: (MonadIO m) => Type -> m Type
tyZonk t =
  tyForce t >>= \case
    TyFun as b -> TyFun <$> traverse tyZonk as <*> tyZonk b
    x -> pure x

tySolve :: (HasCallStack, MonadIO m) => IORef TyMeta -> Type -> m ()
tySolve m t =
  liftIO $
    readIORef m >>= \case
      MUnsolved _ -> writeIORef m (MSolved t)
      _ -> error "solved meta passed to `tySolve`"

tyUnify :: (HasCallStack, MonadIO m) => Type -> Type -> m ()
tyUnify x y = do
  x <- tyForce x
  y <- tyForce y
  go x y
  where
    go x y | x == y = pure ()
    go (TyMeta m) t = tySolve m t
    go t (TyMeta m) = tySolve m t
    go (TyFun as b) (TyFun as' b')
      | length as == length as' =
          sequence_ (mzipWith go as as') >> go b b'
    go x y = do
      x <- tyZonk x
      y <- tyZonk y
      elabError (ErrTypeUnify x y)

tyUnifyLabel :: (HasCallStack, MonadIO m) => Label -> Label -> m ()
tyUnifyLabel (Label _ xs) (Label _ ys)
  | length xs == length ys =
      sequence_ (mzipWith tyUnify xs ys)
  | otherwise = todo

data RawModule = RawModule
  { dataCons :: Map RawName (DataTag, [Type], Type)
  , prims    :: Map RawName (PrimOp, [Type], Type)
  , terms    :: Map RawName Var
  , typeCons :: Map RawName (Name, Set DataTag)
  }
  deriving (Show)

rawBindName :: RawModule -> RawName -> Var -> RawModule
rawBindName RawModule {..} x v =
  RawModule {terms = terms & (at x ?~ v), ..}

rawBindNames ::
  (MonadZip m, Foldable m) =>
  RawModule ->
  m RawName ->
  m Var ->
  RawModule
rawBindNames raw xs vs =
  foldl' (\raw (x, v) -> rawBindName raw x v) raw (mzip xs vs)

data ElabT = Elab
  { raw     :: RawModule
  , level   :: Int
  , tInt    :: Type
  , tChar   :: Type
  , tString :: Type
  }

type Elab r = ReaderT ElabT IO r

elabContext :: Elab RawModule
elabContext = asks (.raw)

-- elabShiftLevel1 :: (Int -> Elab r) -> Elab r
-- elabShiftLevel1 = elabShiftLevel 1

-- elabShiftLevel :: Int -> (Int -> Elab r) -> Elab r
-- elabShiftLevel by k = ReaderT \Elab {..} ->
--   runReaderT (k level) Elab {level = level + by, ..}

-- elabSetLevel :: Int -> Elab r -> Elab r
-- elabSetLevel l k = ReaderT \Elab {..} ->
--   runReaderT k Elab {level = l, ..}

-- elabBindName :: RawName -> Var -> Elab r -> Elab r
-- elabBindName x v = local \Elab {..} ->
--   Elab {raw = rawBindName raw x v, ..}

elabBindName :: RawName -> Type -> (Var -> Elab r) -> Elab r
elabBindName x t k = do
  Elab{..} <- ask
  let v = Var (Bound x level) t
  flip local (k v) \Elab {..} ->
    Elab {raw = rawBindName raw x v, ..}

elabBindNames
  :: (MonadZip m, Foldable m, Traversable m)
  => m RawName -> m Type -> (m Var -> Elab r) -> Elab r
elabBindNames xs ts k = do
  Elab{..} <- ask
  let
    (_, vs) = mapAccumL
      (\i (x, t) -> (i + 1, Var (Bound x i) t))
      level
      (mzip xs ts)
  flip local (k vs) \Elab {..} ->
    Elab {raw = rawBindNames raw xs vs, ..}

-- elabBindNames ::
--   (MonadZip m, Foldable m) =>
--   m RawName ->
--   m Var ->
--   Elab r ->
--   Elab r
-- elabBindNames xs vs = local \Elab {..} ->
--   Elab {raw = rawBindNames raw xs vs, ..}

elabTypeRaw ::
  (HasCallStack, MonadIO m) =>
  Map RawName (Name, Set DataTag) ->
  RawType ->
  m Type
elabTypeRaw ctx = go ctx Empty
  where
    go ctx acc (RTyFun a b) = do
      a' <- go ctx Empty a
      go ctx (acc :|> a') b
    go ctx acc r@(RTyCon c) =
      case ctx ^. at c of
        Nothing -> elabError (ErrTypeUnkown r)
        Just (t, _)
          | null acc -> pure $ TyCon t
          | otherwise -> pure $ TyFun acc (TyCon t)

elabType :: HasCallStack => RawType -> Elab Type
elabType t = do
  ctx <- elabContext
  elabTypeRaw ctx.typeCons t

data Cont where
  Abstract :: Label -> Cont
  Wrap :: (Var -> Elab Term) -> Cont

contApply :: HasCallStack => Cont -> Var -> Elab Term
contApply (Abstract k) x = pure $ TJmp k (pure x)
contApply (Wrap k) x     = k x

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
  RawModule {..} <- elabContext
  case terms ^. at x of
    Nothing -> elabError (ErrVarUnknown x)
    Just v  -> pure v

litType :: HasCallStack => Literal -> Elab Type
litType x = do
  Elab {..} <- ask
  pure case x of
    LInt _    -> tInt
    LChar _   -> tChar
    LString _ -> tString

-- abstractCont k = do
--   case k of
--     Abstract _ -> pure k
--     Wrap k -> elabShiftLevel1 \i -> do
--       let x = Var (Bound "x" i) t
--       kx <- k x
--       k <- freshLabel "k" [t]
--       pure Wrap $ TLetK [(k, [x], kx)] (TApp v k vs)

elabTerms ::
  (HasCallStack, Foldable f) =>
  f Raw ->
  f Type ->
  (Seq Var -> Elab Term) ->
  Elab Term
elabTerms xs ts k = foldr2 c k xs ts Empty
  where
    {-# INLINE c #-}
    c x t k zs = elabTerm x t (Wrap \z -> k (zs :|> z))

elabTerm :: HasCallStack => Raw -> Type -> Cont -> Elab Term
elabTerm (RAnnot x t') t k = do
  elabType t' >>= tyUnify t
  elabTerm x t k
elabTerm (RApp f (Seq.fromList -> rArgs)) t k = do
  tArgs <- mapM (const freshMeta) rArgs
  elabTerm f (TyFun tArgs t) $ Wrap \vFun -> elabTerms rArgs tArgs \vArgs ->
    case k of
      Abstract k -> do
        -- tyUnify (varType k) (TyNeg [t])
        pure $ TApp vFun k vArgs
      Wrap k -> elabBindName "x" t \x -> do
        kx <- k x
        k <- freshLabel "k" [t]
        pure $ TLetK [(k, [x], kx)] (TApp vFun k vArgs)
elabTerm (RCase _x _es) _t _k = do
  -- t' <- freshMeta
  -- elab x t $ Wrap \v ->
  --   case k of
  --     Abstract k -> todo
  --     Wrap k -> todo
  -- where
  --   mkRow k (p, e) = _
  todo
elabTerm (RData rCon rArgs) t k = do
  raw <- elabContext
  case raw.dataCons ^. at rCon of
    Nothing -> elabError (ErrConUnknown rCon)
    Just (dTag, tArgs, tRet) -> do
      let nParams = length tArgs
          nArgs   = length rArgs
      unless (nParams == nArgs) $
        elabError (ErrConArity rCon nParams nArgs)
      elabTerms rArgs tArgs \vArgs -> do
        tyUnify t tRet
        x <- freshLocal "d" tRet
        e <- contApply k x
        pure $ TLetV x (VData dTag vArgs) e
elabTerm (RLambda (Seq.fromList -> rArgs) e) t k = do
  f <- freshLocal "f" t
  tArgs <- mapM (const freshMeta) rArgs
  tRet  <- freshMeta
  tyUnify t (TyFun tArgs tRet)
  j <-elabBindNames rArgs tArgs \vArgs -> do
    let kRet = labelReturn tRet
    e <- elabTerm e tRet (Abstract kRet)
    pure $ TLetF [(f, kRet, vArgs, e)]
  j <$> contApply k f
-- elab (RLet False xs e) t k = do
--   elabCtx <- ask
--   -- TODO: we need to modify the local env somehow for annotations
--   foldM `flip` (elabCtx) `flip` xs $ \cases
--     acc (RTDAnno x t) -> do
--       t <- elabType t
--       v <- freshLocal x t
--       pure (acc { ctx = ctxBindName acc.ctx x v })
--     acc (RTDTerm x e) -> do
--       case acc.ctx.terms ^. at x of
--         Just v  -> pure $ Just (v, e)
--         Nothing -> do
--           t <- freshMeta
--           Just . (,e) <$> freshLocal x t
--   todo
elabTerm (RLet xs _e) _t _k = do
  -- TODO: we need to modify the local env somehow for annotations
  -- _xs <- flip witherM xs \case
  --   RTDAnno x t -> do
  --     t <- elabType t
  --     freshLocal x t $> Nothing
  --   RTDTerm x e -> do
  --     raw <- elabContext
  --     case raw.terms ^. at x of
  --       Just v -> pure $ Just (v, e)
  --       Nothing -> do
  --         t <- freshMeta
  --         Just . (,e) <$> freshLocal x t
  --   -- since this isn't a block of recursive functions, it is a block of values
  --   -- that may include functions, so we split them up into a group of funcions
  --   -- and a group of values
  todo
  where
    go acc = acc
elabTerm (RLetRec _ _) _t _k = todo
elabTerm (RLit l) t k = do
  litType l >>= tyUnify t
  x <- freshLocal "l" t
  e <- contApply k x
  pure $ TLetV x (VLit l) e
elabTerm (RPrim rPrim rArgs) t k = do
  raw <- elabContext
  case raw.prims ^. at rPrim of
    Nothing -> elabError (ErrPrimUnknown rPrim)
    Just (prim, tArgs, tRet) -> do
      let nParams = length tArgs
          nArgs   = length rArgs
      unless (nParams == nArgs) $
        elabError (ErrPrimArity rPrim nParams nArgs)
      elabTerms rArgs tArgs \vArgs -> do
        tyUnify t tRet
        x <- freshLocal "p" tRet
        e <- contApply k x
        pure $ TLetP x prim vArgs e
elabTerm (RVar x) t k = do
  raw <- elabContext
  case raw.terms ^. at x of
    Nothing -> elabError (ErrVarUnknown x)
    Just v -> do
      tyUnify t (varType v)
      contApply k v

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

--- COMPILATION

data Module = Module
  { dataCons :: NameMap (DataTag, Type)
  , prims    :: NameMap ([Type], Type)
  , terms    :: NameMap (Term, Var)
  , typeCons :: NameMap (Set DataTag)
  }
  deriving (Show)

rawModule :: (HasCallStack, MonadIO m) => [RawDef] -> m ([(Var, Raw)], RawModule)
rawModule rDefs = do
  typeCons <- foldM `flip` mempty `flip` rDefs $ \cases
    typeCons (RDData raw _) -> do
      name <- checkDup "type constructor" typeCons raw
      pure $ typeCons & at raw ?~ (name, [])
    typeCons _ -> pure typeCons

  foldM fDef `flip` rDefs $
    ( []
    , RawModule
        { dataCons = mempty
        , prims    = mempty
        , terms    = mempty
        , typeCons = typeCons
        }
    )
  where
    checkDup thing map rName = do
      when (isJust $ map ^. at rName) $ do
        elabError (ErrDefDuplicate thing rName)
      freshGlobalName rName

    fCon tCon tParent (conSet, dataCons) (rName, rArgs, rRet) = do
      name <- checkDup "data constructor" dataCons rName
      tArgs <- mapM (elabTypeRaw tCon) rArgs
      tRet <- elabTypeRaw tCon rRet
      unless (tRet == tParent) $
        elabError (ErrDefOther "bad data constructor")
      let tag = DataTag name (TrivialOrd $ length conSet)
          conSet' = Set.insert tag conSet
          result = (tag, tArgs, tRet)
      pure (conSet', dataCons & at rName ?~ result)

    fDef (rTerms, RawModule{..}) (RDData rName rCons) = do
      let (name, _) = typeCons Map.! rName
      (conSet, dataCons) <-
        foldM
          (fCon typeCons (TyCon name))
          (mempty, dataCons)
          rCons
      pure
        ( rTerms
        , RawModule
            { typeCons = typeCons & at rName ?~ (name, conSet)
            , dataCons
            , ..
            }
        )

    fDef (rTerms, RawModule{..}) (RDPrim rName rType) = do
      name <- checkDup "primitive function" prims rName
      (tArgs, tRet) <- typeSplit <$> elabTypeRaw typeCons rType
      pure
        ( rTerms
        , RawModule{prims = prims & at rName ?~ (name, tArgs, tRet), ..}
        )

    fDef (rTerms, RawModule{..}) (RDTerm rName rType rTerm) = do
      name <- checkDup "top-level" terms rName
      v <- Var name <$> elabTypeRaw typeCons rType
      pure
        ( (v, rTerm) : rTerms
        , RawModule{terms = terms & at rName ?~ v, ..}
        )

elaborate :: (MonadIO m) => [RawDef] -> m Module
elaborate xs = liftIO do
  (xs, raw) <- rawModule xs
  tInt <- getType raw.typeCons "Int"
  tString <- getType raw.typeCons "String"
  tChar <- getType raw.typeCons "Char"
  let elab = Elab{level = 1, ..}
  terms <- NameMap . IntMap.fromList <$> forM xs \(v, e) -> do
    let Var n t  = v
        (_, ret) = typeSplit t
        retK     = Abstract $ Label (Bound "k" 0) [ret]
    x <- runReaderT (elabTerm e t retK) elab
    pure (n.id, (x, v))
  pure
    Module
      { dataCons = convert raw.dataCons \(t, _, ret) -> (t.name.id, (t, ret))
      , prims    = convert raw.prims \(p, args, ret) -> (p.id, (args, ret))
      , terms    = terms
      , typeCons = convert raw.typeCons \(t, cons) -> (t.id, cons)
      }
  where
    convert x f = NameMap . IntMap.fromAscList $ Map.elems x <&> f

    getType typeCons t =
      case fst <$> (typeCons ^. at t) of
        Nothing -> elabError (ErrTypeUnkown (RTyCon t))
        Just t  -> pure (TyCon t)
