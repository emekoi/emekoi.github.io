---
title: Complete and Easy
published: 2024-11-05
slug: complete-and-easy
---

::: {.details}
Haskell language extensions and module imports.

```haskell
module Index ( main ) where

import Control.Applicative
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Control.Exception                        (Exception (..), throwIO)
import Control.Exception                        (handle)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class                   (MonadIO (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Char                                qualified as Char
import Data.Foldable                            (Foldable (foldl'))
import Data.Functor
import Data.IORef                               (IORef)
import Data.IORef                               qualified as IORef
import Data.List                                qualified as List
import Data.List.NonEmpty                       (NonEmpty (..))
import Data.Map.Strict                          (Map)
import Data.Map.Strict                          qualified as Map
import Data.Set                                 qualified as Set
import Data.Text                                qualified as Text
import Data.Text.IO                             qualified as Text
import Data.Typeable                            (Typeable)
import Data.Void                                (Void)
import GHC.Stack
import Prelude                                  hiding ((!!), unzip)
import System.Environment                       (getArgs)
import Text.Megaparsec                          qualified as Mega
import Text.Megaparsec.Char                     qualified as Mega
import Text.Megaparsec.Char.Lexer               qualified as MegaL
import Data.Unique
```
:::

# Utilities

Before writing any inference code, we define some helpers. The functions and
alias are self-explanatory, but the purpose of the paramter `m` in `Display m x`
is to allow `display` to use effects if needed.

``` haskell
type StrictText = Text.Text

class Display m x where
  display :: x -> m StrictText

-- | a flipped version of `zipWithM`
zipWithM' :: Applicative m => [a] -> [b] -> (a -> b -> m c) -> m [c]
zipWithM' xs ys f = zipWithM f xs ys

-- | `(>>=)` lifted to binary functions
bind2 :: Monad m => m a -> m b -> (a -> b -> m c) -> m c
bind2 x y k = x >>= ((y >>=) . k)

-- | `readIORef` lifted to `MonadIO`
readIORef :: MonadIO m => IORef a -> m a
readIORef = liftIO . IORef.readIORef

-- | `writeIORef` lifted to `MonadIO`
writeIORef :: MonadIO m => IORef a -> a -> m ()
writeIORef h = liftIO . IORef.writeIORef h
```

At this point, we might as well also define our error handling situation. Since
we will be making use of `IO` and all errors are fatal, we will be using
exceptions as well. In a real implementaion, you might want to use some
combination of `ST` and `Either` (or `unsafePerformIO`).

``` haskell
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
  | TypeErrorExprNonFunction StrictText
  deriving (Show, Typeable)

type Dbg = HasCallStack

data TypeError where
  TypeError :: Dbg => TypeErrorKind -> TypeError

instance Show TypeError where
  show (TypeError x) = show x ++ "\n" ++ prettyCallStack callStack

instance Exception TypeError where

throw :: (Dbg, MonadIO m) => TypeErrorKind -> m a
throw x = liftIO . throwIO $ withFrozenCallStack (TypeError x)
```

# Base Syntax

With those helpers defined, we can now define our raw surface syntax:

``` haskell
type Name = Text.Text

data RKind where
  RKType  :: RKind
  RKArrow :: [RKind] -> RKind -> RKind
  deriving (Eq, Show)

data RType where
  RTCon    :: Name -> RType
  RTVar    :: Name -> RType
  RTArrow  :: [RType] -> RType -> RType
  RTApply  :: RType -> [RType] -> RType
  RTForall :: Name -> Maybe RKind -> RType -> RType
  deriving (Eq, Show)

data Expr where
  EUnit   :: Expr
  EInt    :: Int -> Expr
  EVar    :: Name -> Expr
  ELambda :: [(Name, Maybe RType)] -> Expr -> Expr
  ELet    :: Name -> Maybe RType -> Expr -> Expr -> Expr
  ELetRec :: Name -> Maybe RType -> Expr -> Expr -> Expr
  EApply  :: Expr -> [Expr] -> Expr
  EAnnot  :: Expr -> RType -> Expr
  deriving (Eq, Show)
```

Aside from the fact that constructs are uncurried when possible, there should be
nothing surprising. In order to define our elaborated syntax, we must first
define de Bruijn indicies and levels and some related functions:

``` haskell
newtype Level
  = Level { unLevel :: Int }
  deriving (Enum, Eq, Ord, Show)

newtype Index
  = Index { unIndex :: Int }
  deriving (Enum, Eq, Ord, Show)

-- | convert a level to an index with a given base level
lvl2idx :: Level -> Level -> Index
lvl2idx (Level l) (Level x) = Index (l - x - 1)

-- | index into a list using a level
(!!) :: Dbg => [a] -> Index -> a
xs !! (Index i) = xs List.!! i
```

Then we may define our kinds

``` haskell
data KHole where
  KHFull  :: Kind -> KHole
  KHEmpty :: KHole
  deriving (Eq)

data Kind where
  KType  :: Kind
  KHole  :: IORef KHole -> Kind
  KArrow :: [Kind] -> Kind -> Kind
  deriving (Eq)
```

And our types:

``` haskell
data THole where
  THFull  :: VType -> THole
  THEmpty :: Name -> Kind -> Level -> Unique -> THole
  deriving (Eq)

data TType where
  TTCon    :: Name -> Kind -> TType
  TTVar    :: Index -> Kind -> TType
  TTArrow  :: [TType] -> TType -> TType
  TTApply  :: TType -> [TType] -> TType
  TTForall :: Name -> Kind -> TType -> TType
  TTHole   :: IORef THole -> TType
  deriving (Eq)

data VType where
  VTCon    :: Name -> Kind -> VType
  VTVar    :: Level -> Kind -> VType
  VTArrow  :: [VType] -> VType -> VType
  VTApply  :: VType -> [VType] -> VType
  VTForall :: Name -> Kind -> [VType] -> TType -> VType
  VTHole   :: IORef THole -> VType
  deriving (Eq)
```

Again, both kinds and type are uncurried. 

# Type Unification

Once again, we have a large amount of boilerplate to cover before we can get to
the actually interesting bits. First we define our typechecking monad as
`ReaderT Context IO`. Since unification occurs in `IO`, we only need `Reader`
and not `State`.

``` haskell
data Context = Context
  { typeLevel     :: Level
    -- ^ the current level
  , rawTypeNames  :: [Name]
    -- ^ stack of names from binders passed so far (length = typeLevel)
  , typeEnv       :: [VType]
    -- ^ environment for evaluation of types (length = typeLevel)
  , rawTypeLevels :: Map Name (Level, Kind)
    -- ^ map from names to the levels they were declared at and their kind
  , rawTypeCons   :: Map Name Kind
    -- ^ map from names to type constructors
  , rawTermTypes  :: Map Name VType
    -- ^ map from term variables to types
  }

type Check = ReaderT Context IO

runCheck :: Dbg => Check m -> IO m
runCheck m = do
  runReaderT m Context
    { typeLevel     = Level 0
    , rawTypeNames  = mempty
    , typeEnv       = mempty
    , rawTypeLevels = mempty
    , rawTypeCons   = Map.fromList
      [ ("Unit", KType)
      , ("Int", KType)
      ]
    , rawTermTypes  = mempty
    }
```

Next we define some utilities for running monadic actions with variables bound.

``` haskell
-- | run a action with a type variable bound to a given type
typeVarBind :: Dbg => Name -> Kind -> Check r -> Check r
typeVarBind x k = local \Context{..} -> Context
  { typeLevel     = succ typeLevel
  , rawTypeLevels = Map.insert x (typeLevel, k) rawTypeLevels
  , rawTypeNames  = x : rawTypeNames
  , typeEnv       = VTVar typeLevel k : typeEnv
  , ..
  }

-- | run a action with a term variable bound to a given type
exprVarBind :: Dbg => Name -> VType -> Check r -> Check r
exprVarBind x t = local \ctx -> ctx
  { rawTermTypes = Map.insert x t (rawTermTypes ctx)
  }
```

Now we defined some functions for creating holes and dereferencing them while
performing path compression.

``` haskell
kindHole :: Check Kind
kindHole = liftIO $ KHole <$> IORef.newIORef KHEmpty

kindForce :: (Dbg, MonadIO m) => Kind -> m Kind
kindForce (KHole h) = go pure h
  where
    go k h = readIORef h >>= \case
      KHFull (KHole h') -> 
        go (\t -> writeIORef h (KHFull t) *> k t) h'
      KHFull t  -> k t
      KHEmpty{} -> k (KHole h)
kindForce t = pure t

typeHole :: Name -> Kind -> Level -> Check VType
typeHole x k l = liftIO $ do
  u <- newUnique
  r <- IORef.newIORef (THEmpty x k l u)
  pure $ VTHole r

typeForce :: (Dbg, MonadIO m) => VType -> m VType
typeForce (VTHole h) = go pure h
  where
    go k h = readIORef h >>= \case
      THFull (VTHole h') ->
        go (\t -> writeIORef h (THFull t) *> k t) h'
      THFull t  -> k t
      THEmpty{} -> k (VTHole h)
typeForce t = pure t
```

``` haskell
typeEval :: (Dbg, MonadIO m) => [VType] -> TType -> m VType
typeEval env (TTForall x k t) = 
  pure $ VTForall x k env t
typeEval env (TTVar i _)      = 
  pure $ env !! i
typeEval _   (TTCon c k)      = 
  pure $ VTCon c k
typeEval env (TTArrow t1s t2) = 
  VTArrow <$> mapM (typeEval env) t1s <*> typeEval env t2
typeEval env (TTApply t1 t2s) = 
  VTApply <$> typeEval env t1 <*> mapM (typeEval env) t2s
typeEval _   (TTHole h)       = 
  typeForce (VTHole h)

typeQuote :: (Dbg, MonadIO m) => Level -> VType -> m TType
typeQuote l t = typeForce t >>= \case
  VTForall x k env t -> do
    t' <- typeEval (VTVar l k : env) t
    TTForall x k <$> typeQuote (succ l) t'
  VTVar x k -> pure $ TTVar (lvl2idx l x) k
  VTCon c k -> pure $ TTCon c k
  VTArrow t1s t2 -> TTArrow <$> mapM (typeQuote l) t1s <*> typeQuote l t2
  VTApply t1 t2s -> TTApply <$> typeQuote l t1 <*> mapM (typeQuote l) t2s
  VTHole h -> pure $ TTHole h
```

``` haskell
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

    scopeCheck _ KType = pure ()
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
        KHEmpty  -> pure ()
```

``` haskell
typeUnify :: Dbg => VType -> VType -> Check ()
typeUnify _t1 _t2 = bind2 (typeForce _t1) (typeForce _t2) \cases
  t1 t2 | t1 == t2 -> pure ()
  (VTHole t1) t2 -> readIORef t1 >>= \case
    THEmpty _ k l _ -> fillHole l k t1 t2
    THFull t1       -> typeUnify t1 t2
  t1 (VTHole t2) -> readIORef t2 >>= \case
    THEmpty _ k l _ -> fillHole l k t2 t1
    THFull t2       -> typeUnify t1 t2
  (VTForall x1 k1 e1 t1) (VTForall _ k2 e2 t2) -> do
    kindUnify k1 k2
    l <- asks typeLevel
    typeVarBind x1 k1 do
      t1 <- typeEval (VTVar l k1 : e1) t1
      t2 <- typeEval (VTVar l k2 : e2) t2
      typeUnify t1 t2
  (VTVar l1 k1) (VTVar l2 k2) | l1 == l2 ->
    kindUnify k1 k2
  (VTCon n1 k1) (VTCon n2 k2) | n1 == n2 ->
    kindUnify k1 k2
  (VTArrow t1s t1) (VTArrow t2s t2) | length t1s == length t2s ->
    zipWithM_ typeUnify t1s t2s *> typeUnify t1 t2
  (VTApply t1 t1s) (VTApply t2 t2s) | length t1s == length t2s ->
    typeUnify t1 t2 *> zipWithM_ typeUnify t1s t2s
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
      typeVarBind x k $ typeEval (v k : env) t
        >>= scopeCheck l l' h
    scopeCheck l l' _ t@(VTVar x k) = do
      when (l <= x && x < l') do
        s <- display t
        throw (TypeErrorTypeVariableEscape s)
      pure k
    scopeCheck _ _ _ (VTCon _ k) = pure k
    scopeCheck l l' h (VTArrow as b) = do
      mapM_ (scopeCheck l l' h >=> kindUnify KType) as
      scopeCheck l l' h b >>= kindUnify KType
      pure KType
    scopeCheck l l' h (VTApply f xs) =
      scopeCheck l l' h f >>= kindForce >>= \case
        KArrow k1 k2 -> do
          let (lk, lx) = (length k1, length xs)
          void $ zipWithM' k1 xs \k t -> do
            scopeCheck l l' h t >>= kindUnify k
          if lk > lx then pure $ KArrow (drop lx k1) k2
          else if lk == lx then pure k2
          else do
            s <- display (VTApply f (take lk xs))
            throw $ TypeErrorKindNonArrow s
        KHole h' -> do
          k1 <- mapM (scopeCheck l l' h) xs
          k2 <- kindHole
          writeIORef h' (KHFull (KArrow k1 k2)) $> k2
        _ -> do
          s <- display f
          throw $ TypeErrorKindNonArrow s
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
```

# Type Inference

``` haskell
typeCheck' :: Dbg => RType -> Check (VType, TType, Kind)
typeCheck' t = do
  raw <- asks rawTypeLevels
  env <- asks typeEnv
  go (Level $ length raw, raw, env) t
  where
    getKind RKType         = KType
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
      Just k  -> pure (VTCon c k, TTCon c k, k)
    go acc (RTArrow rt1s rt2) = do
      (vt1s, tt1s) <- unzip <$> forM rt1s \rt -> do
        (vt, tt, k) <- go acc rt
        kindUnify k KType $> (vt, tt)
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
          else if lk == lrt then 
            pure (VTApply vt1 vt2s, TTApply tt1 tt2s, k2)
          else do
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
        KHole h' -> do
          (vt2s, tt2s, k1s) <- unzip3 <$> mapM (go acc) rt2s
          k2 <- kindHole
          writeIORef h' (KHFull (KArrow k1s k2)) $>
            (VTApply vt1 vt2s, TTApply tt1 tt2s, k2)
        _ -> do
          s <- display tt1
          throw $ TypeErrorKindNonArrow s

typeInfer :: Dbg => RType -> Check VType
typeInfer t = do
  (v, _, k) <- typeCheck' t
  kindUnify KType k
  pure v

```


``` haskell
exprCheck :: Dbg => Expr -> VType -> Check ()
exprCheck e t = typeForce t >>= \case
  VTForall x k env t -> do
    l <- asks typeLevel
    t' <- typeEval (VTVar l k : env) t
    typeVarBind x k $ exprCheck e t'
  VTArrow ss t | ELambda xs e <- e -> do
    let (ls, lx) = (length ss, length xs)
    xs' <- zipWithM' ss xs \s (x, s') -> do
      forM_ s' (typeInfer >=> typeUnify s)
      pure (x, s)
    if ls > lx then
      foldr (uncurry exprVarBind) (exprCheck e (VTArrow (drop lx ss) t)) xs'
    else if ls < lx then do
      foldr (uncurry exprVarBind) (exprCheck (ELambda (drop ls xs) e) t) xs'
    else
      foldr (uncurry exprVarBind) (exprCheck e t) xs'
  t2 | ELet x t1 e1 e2 <- e -> do
    t1 <- case t1 of
      Nothing ->
        exprInfer e1
      Just t1 -> do
        t1 <- typeInfer t1
        exprCheck e1 t1 $> t1
    exprVarBind x t1 $ exprCheck e2 t2
  t2 | ELetRec x t1 e1 e2 <- e -> do
    t1 <- maybe (asks typeLevel >>= typeHole x KType) typeInfer t1
    exprVarBind x t1 do
      exprCheck e1 t1
      exprCheck e2 t2
  t -> do
    t' <- exprInferInst e
    typeUnify t' t

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
  t <- typeInfer t
  exprCheck e t $> t
exprInfer (ELambda xs e) = do
  l <- asks typeLevel
  ts <- forM xs \(x, t) ->
   (x,) <$> maybe (typeHole x KType l) typeInfer t
  r <- foldr (uncurry exprVarBind) (exprInferInst e) ts
  pure $ VTArrow (snd <$> ts) r
exprInfer (EApply f xs) =
  exprInferInst f >>= typeForce >>= \case
    VTArrow ts r -> apply ts r [] xs
    t -> apply [] t [] xs
  where
    apply (t:ts) r xs (y:ys) = exprCheck y t *> apply ts r (y : xs) ys
    apply [] r _ []         = pure r
    apply ts r _ []         = pure $ VTArrow ts r
    apply [] r xs ys        = typeForce r >>= \case
      VTArrow ts r -> apply ts r xs ys
      VTHole h -> readIORef h >>= \case
        THFull _ -> error "IMPOSSIBLE"
        THEmpty n k l _ -> do
          ts <- mapM exprInferInst ys
          r <- typeHole n k l
          writeIORef h (THFull (VTArrow ts r)) $> r
      _ -> throw . TypeErrorExprNonFunction . Text.pack $ show (EApply f (reverse xs))
exprInfer (ELet x t1 e1 e2) = do
  t1 <- case t1 of
    Nothing ->
      exprInfer e1
    Just t1 -> do
      t1 <- typeInfer t1
      exprCheck e1 t1 $> t1
  exprVarBind x t1 (exprInfer e2)
exprInfer (ELetRec x t1 e1 e2) = do
  t1 <- maybe (asks typeLevel >>= typeHole x KType) typeInfer t1
  exprVarBind x t1 do
    exprCheck e1 t1
    exprInfer e2

exprInferInst :: Dbg => Expr -> Check VType
exprInferInst e = do
  l <- asks typeLevel
  exprInfer e >>= typeForce >>= instAll l
  where
    instAll l (VTForall x k env t) = do
      h <- typeHole x k l
      typeEval (h : env) t >>= instAll l
    instAll _ t = pure t
```

With our core typechecking code written, we can now write code for checking
top-level expressions.

``` haskell
exprTopCheck :: Dbg => Expr -> RType -> Check TType
exprTopCheck e t = do
  t <- typeInfer t
  exprCheck e t
  asks typeLevel >>= flip typeQuote t
```

Type inference for top level expresssions is b 

``` haskell
exprTopInfer :: Dbg => Expr -> Check TType
exprTopInfer e = do
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
      VTCon c k -> pure $ TTCon c k
      VTArrow ss t -> TTArrow <$> mapM (go l) ss <*> go l t
      VTApply f xs -> TTApply <$> go l f <*> mapM (go l) xs
      VTHole h -> do
        readIORef h >>= \case
          THEmpty n k _ _ -> StateT \vs -> do
            let vl = Level (length vs)
            writeIORef h (THFull (VTVar vl k))
            pure ( TTVar (lvl2idx l vl) k
                 , (n <> Text.pack (show (unLevel vl)), k) : vs
                 )
          THFull t -> go l t
```

# Extra Credit

```haskell
main :: IO ()
main = do
  (file, input) <- getArgs >>= \case
    file : _ -> (file, ) <$> Text.readFile file
    _ -> ("<stdin>",) <$> Text.getContents

  case Mega.runParser parser file input of
    Left  x  -> putStrLn (Mega.errorBundlePretty x)
    Right xs -> runCheck $ forM_ xs \(x, t, e) -> wrap do
      s <- display =<< case t of
        Just t  -> exprTopCheck e t
        Nothing -> exprTopInfer e
      liftIO . Text.putStrLn $ x <> " : " <> s
      where
        wrap m = ReaderT $
          handle @TypeError (\x -> print x *> putChar '\n') . runReaderT m
```

::: {.details}
Pretty Printer

``` haskell
instance (MonadIO m) => Display m Kind where
  display = go False
    where
      parens True x  = "(" <> x <> ")"
      parens False x = x

      go _ KType = pure "Type"
      go p (KArrow xs r) = do
        xs <- mapM (go True) xs
        r <- go False r
        pure $ parens p (Text.intercalate " -> " (xs ++ [r]))
      go p (KHole h) = readIORef h >>= \case
        KHEmpty -> pure "?"
        KHFull k -> go p k

data TTypeParen
  = ParenNone
  | ParenArrow
  | ParenApply

instance Display Check TType where
  display = go ParenNone
    where
      parenApply ParenApply x = "(" <> x <> ")"
      parenApply _ x          = x

      parens ParenNone x = x
      parens _ x         = "(" <> x <> ")"

      displayUnique = Text.pack . show . hashUnique

      go p (TTForall x k t) = do
        t <- typeVarBind x k $ go ParenNone t
        x <- kindForce k >>= \k -> do
          k <- display k
          pure $ "(" <> x <> " : " <> k <>  ")"
        pure $ parens p ("forall " <> x <> ". " <> t)
      go _ (TTVar i _) = (!! i) <$> asks rawTypeNames
      go _ (TTCon c _) = pure c
      go p (TTArrow as b) = do
        as <- mapM (go ParenArrow) as
        b <- go ParenNone b
        pure $ parens p (Text.intercalate " -> " (as ++ [b]))
      go p (TTApply f xs) = do
        f <- go ParenNone f
        xs <- mapM (go ParenApply) xs
        pure $ parenApply p (Text.intercalate " " (f : xs))
      go p (TTHole h) = readIORef h >>= \case
        THEmpty x k _ u -> do
          k <- display k
          pure $ Text.cons '?' k <> displayUnique u <> "." <> x
        THFull t -> do
          l <- asks typeLevel
          t <- typeQuote l t
          go p t

instance Display Check VType where
  display t = do
    l <- asks typeLevel
    typeQuote l t >>= display
```
:::

::: {.details}
Parser

```haskell
type Parsec = Mega.Parsec Void StrictText

tabWidth :: Mega.Pos
tabWidth = Mega.mkPos 2

lineFold1 :: Parsec a -> Parsec (NonEmpty a)
lineFold1 p = space >> do
  pos <- Mega.getSourcePos
  let
    refLine = Mega.sourceLine pos
    refCol  = tabWidth <> Mega.sourceColumn pos
    space' = guard refLine refCol
  (p `sepBy1` Mega.try space') <* space
  where
    guard refLine refCol = space >> do
      pos <- Mega.getSourcePos
      let
        line = Mega.sourceLine pos
        col  = Mega.sourceColumn pos
      unless (line == refLine || col >= refCol) $
        MegaL.incorrectIndent EQ refCol col

identChar :: Char -> Bool
identChar c = Char.isAlphaNum c || c == '_' || c == '?' || c == '\''

lineComment, blockComment :: Parsec ()
lineComment  = MegaL.skipLineComment "--"
blockComment = MegaL.skipBlockComment "{-" "-}"

space :: Parsec ()
space = MegaL.space Mega.space1 lineComment blockComment

nbspace :: Parsec ()
nbspace = MegaL.space space1 lineComment blockComment
  where
    space1 = void $ Mega.takeWhile1P (Just "white space") isSpace
    isSpace c = c /= '\n' && Char.isSpace c

symbol :: StrictText -> Parsec StrictText
symbol = MegaL.symbol space

lexeme :: Parsec a -> Parsec a
lexeme = MegaL.lexeme space

nblexeme :: Parsec a -> Parsec a
nblexeme = MegaL.lexeme nbspace

parens :: Parsec a -> Parsec a
parens = Mega.between (symbol "(") (symbol ")")

rsymbol :: StrictText -> Parsec ()
rsymbol w = (nblexeme . Mega.try)
  (Mega.string w *> Mega.notFollowedBy Mega.alphaNumChar)

ident :: Parsec Char -> Parsec StrictText
ident first = nblexeme . Mega.try $ (name >>= check)
  where
    firstChar = first <|> Mega.char '_'
    keywords = Set.fromList ["let", "in", "λ", "→", "forall", "∀"]
    name = Mega.lookAhead firstChar *> Mega.takeWhileP Nothing identChar
    check x
      | x `Set.member` keywords = fail $ show x ++ " cannot be an identifier"
      | otherwise = pure x

pVar :: Parsec StrictText
pVar =  ident Mega.lowerChar

pCon :: Parsec StrictText
pCon = ident Mega.upperChar

pArrow :: Parsec ()
pArrow = void $ symbol "->" <|> symbol "→"

pKind :: Parsec RKind
pKind =
  sepBy1 pAtom pArrow >>= pure . \case
    x :| [] -> x
    -- we know xs is nonempty so init and last are fine
    x :| xs -> case last xs of
      RKArrow xs' y -> RKArrow (x : (init xs ++ xs')) y
      y             -> RKArrow (x : init xs) y
  where
    pAtom = Mega.choice
      [ parens pKind
      , rsymbol "Type" $> RKType
      ]

pArg :: Parsec p -> Parsec (StrictText, Maybe p)
pArg p = (, Nothing) <$> pVar <|> parens do
  (,) <$> pVar <*> fmap Just (symbol ":" *> p)

pType :: Parsec RType
pType = pForall <|> pType2
  where
    pAtom = Mega.choice
      [ parens pType
      , RTCon <$> nblexeme pCon
      , RTVar <$> pVar
      ]

    pType1 = do
      xs <- lineFold1 pAtom
      pure case xs of
        x :| [] -> x
        f :| xs -> case f of
          RTApply f xs' -> RTApply f (xs' ++ xs)
          _             -> RTApply f xs

    pType2 = do
      sepBy1 pType1 pArrow >>= pure . \case
        x :| [] -> x
        -- we know xs is nonempty so init and last are fine
        x :| xs -> case last xs of
          RTArrow xs' y -> RTArrow (x : (init xs ++ xs')) y
          y             -> RTArrow (x : init xs) y

    pForall = do
      rsymbol "forall" <|> rsymbol "∀"
      (x, t) <- pArg pKind <* symbol "."
      RTForall x t <$> pType

pExpr :: Parsec Expr
pExpr = do
  x <- pExpr1
  Mega.optional (symbol ":" *> pType) <&> \case
    Just t -> EAnnot x t
    Nothing -> x
  where
    pAtom = Mega.choice
      [ parens pExpr
      , pLet
      , pLambda
      , lexeme pInt
      , rsymbol "Unit" $> EUnit
      , EVar <$> pVar
      ]

    pNum = Mega.try (Mega.char '0' >> Mega.choice
      [ Mega.char' 'b' >> MegaL.binary
      , Mega.char' 'o' >> MegaL.octal
      , Mega.char' 'x' >> MegaL.hexadecimal
      ]) <|> MegaL.decimal

    pInt = Mega.try do
      EInt <$> MegaL.signed empty pNum Mega.<?> "integer literal"

    pExpr1 = do
      xs <- lineFold1 pAtom
      pure case xs of
        x :| [] -> x
        f :| xs -> case f of
          EApply f xs' -> EApply f (xs' ++ xs)
          _            -> EApply f xs

    pLet = do
      rsymbol "let"
      isRec <- Mega.option False (rsymbol "rec" $> True)
      x <- pVar
      t <- optional (symbol ":" *> pType)
      v <- symbol "=" *> pExpr
      e <- rsymbol "in" *> pExpr
      pure $ (if isRec then ELetRec else ELet) x t v e

    pLambda = do
      void . lexeme $ (Mega.char '\\' <|> Mega.char 'λ')
      xs <- many (pArg pType) <* pArrow
      ELambda xs <$> pExpr

pDef :: Parsec (StrictText, Maybe RType, Expr)
pDef = MegaL.nonIndented space pTerm
  where
    pTerm = do
      x <- pVar
      t <- optional do
        t <- symbol ":" *> pType
        void . MegaL.nonIndented space . nblexeme $ Mega.chunk x
        pure t
      e <- symbol "=" *> lexeme pExpr
      pure (x, t, e)

parser :: Parsec [(StrictText, Maybe RType, Expr)]
parser = many (lexeme pDef) <* Mega.eof
```
:::
