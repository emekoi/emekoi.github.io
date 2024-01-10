{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}

module Core
    ( module Core
    , module Export
    ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor           (($>))
import Data.Sequence          (Seq (..))
import Data.Text              (Text)
import Parser                 as Export (HasInfo (..), HasRange, Range (..),
                                         range)
import Prettyprinter          (Pretty (..), (<+>))
import Prettyprinter          qualified as P
import Util

newtype TrivialEq r
  = TrivialEq r
  deriving (Show)
    via r
  deriving (Foldable, Functor, Traversable)

instance Eq (TrivialEq r) where
  (==) _ _ = True

instance Ord (TrivialEq r) where
  compare _ _ = EQ

newtype TrivialOrd r
  = TrivialOrd r
  deriving (Show)
    via r
  deriving (Eq, Foldable, Functor, Traversable)

instance (Eq r) => Ord (TrivialOrd r) where
  compare _ _ = EQ

newtype Trivial r
  = Trivial r
  deriving (Foldable, Functor, Traversable)

instance Eq (Trivial r) where
  (==) _ _ = True

instance Ord (Trivial r) where
  compare _ _ = EQ

instance Show (Trivial a) where
  show _ = "<trivial>"

data NameKind
  -- deBruijn indicies
  = BoundName
  -- globally unique (module)
  | LocalName
  -- globally unique (module)
  | GlobalName
  deriving (Eq)

data Name = Name
  { kind    :: TrivialOrd NameKind
  , display :: TrivialEq Text
  , id      :: Int
  }
  deriving (Eq, Ord)

pattern Bound, Local, Global :: Text -> Int -> Name
pattern Bound x i = Name (TrivialOrd BoundName) (TrivialEq x) i
pattern Local x i = Name (TrivialOrd LocalName) (TrivialEq x) i
pattern Global x i = Name (TrivialOrd GlobalName) (TrivialEq x) i
{-# COMPLETE Local, Global, Bound #-}

instance Pretty Name where
  pretty (Bound x i)  = "%" <> P.pretty x <> P.pretty i
  pretty (Local x i)  = "$" <> P.pretty x <> P.pretty i
  pretty (Global x i) = "@" <> P.pretty x <> P.pretty i

instance Show Name where
  show = show . P.pretty

data Type
  = TyFun (Seq Type) Type
  | TyMeta Int (Trivial (IORef (Maybe Type)))
  | TyData TypeId
  deriving (Eq, Ord)

instance Show Type where
  show = show . P.pretty

instance Pretty Type where
  pretty = go False
    where
      go _ (TyMeta i _)         = "?" <> P.pretty i
      go _ (TyData (TypeId c))  = case c.display of TrivialEq c -> P.pretty c
      go False (TyFun args ret) =
        P.concatWith (\x y -> x P.<+> "->" P.<+> y) (go True <$> args)
         P.<+> "->" P.<+> go False ret
      go True x                = P.parens $ go False x

tyUnify :: (MonadIO m) => Type -> Type -> m Bool
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

tyForce :: (MonadIO m) => Type -> m Type
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

class Zonk t where
  zonk :: (MonadIO m) => t -> m t

instance (Traversable t, Zonk t') => Zonk (t t') where
  zonk = traverse zonk

instance Zonk Type where
  zonk t@(TyMeta _ (Trivial m)) =
    readIORef m >>= \case
      Just m -> zonk m
      _ -> pure t
  zonk (TyFun a b)  = TyFun <$> zonk a <*> zonk b
  zonk x            = pure x

tyZonk :: (MonadIO m) => Type -> m Type
tyZonk = zonk

newtype TypeId
  = TypeId Name
  deriving (Eq, Ord, Show)
    via Name

data TypeInfo = TypeInfo
  { name  :: Name
  , range :: Range
  , span  :: Maybe Int
  }
  deriving (Show)

instance HasInfo TypeInfo Range where
  info t = t.range

tyInfo2Ty :: TypeInfo -> Type
tyInfo2Ty t = TyData (TypeId t.name)

newtype DataId
  = DataId Name
  deriving (Eq, Ord, Show)
    via Name

data DataInfo = DataInfo
  { name   :: Name
  , range  :: Range
  , fields :: Seq Type
  , typeOf :: TypeId
  }
  deriving (Show)

instance HasInfo DataInfo Range where
  info t = t.range

data Var = MkVar Name (TrivialEq Type)
  deriving (Eq, Ord)

pattern Var :: Name -> Type -> Var
pattern Var n t = MkVar n (TrivialEq t)
{-# COMPLETE Var :: Var #-}

instance Show Var where
  show (Var v _) = show v

instance Pretty Var where
  pretty (Var v t) = P.parens $ pretty v <+> ":" <+> pretty t

instance Zonk Var where
  zonk (Var v t) = Var v <$> zonk t

data Label = MkLabel Name (TrivialEq (Seq Type))
  deriving (Eq, Ord)

pattern Label :: Name -> Seq Type -> Label
pattern Label n args = MkLabel n (TrivialEq args)
{-# COMPLETE Label :: Label #-}

instance Show Label where
  show (Label v _) = show v

instance Pretty Label where
  pretty (Label v t) = P.parens $
    pretty v <+> ":" <+> P.concatWith (\x y -> x <+> "->" <+> y) (P.pretty <$> t)

instance Zonk Label where
  zonk (Label v ts) = Label v <$> zonk ts

data Value where
  -- | int values
  VInt :: Integer -> Value
  -- | string values
  VString :: Text -> Value
  -- | tagged union from algebraic data types
  VData :: DataId -> Seq Var -> Value
  deriving (Show)

instance Pretty Value where
  pretty (VInt i) = pretty i
  pretty (VString s) = P.dquotes $ P.pretty s
  pretty (VData (DataId c) vs) =
    P.concatWith (<+>) (pretty c :<| (P.pretty <$> vs))

instance Zonk Value where
  zonk (VData d vs) = VData d <$> zonk vs
  zonk v            = pure v

-- TODO: merge this with `Value`?
data AltCon
  = AltInt Integer
  | AltString Text
  | AltData DataId (TrivialEq (Seq Var))
  deriving (Eq, Ord, Show)

instance Pretty AltCon where
  pretty (AltInt i) = pretty i
  pretty (AltString s) = P.dquotes $ P.pretty s
  pretty (AltData (DataId c) (TrivialEq vs)) =
    P.concatWith (<+>) (pretty c :<| (P.pretty <$> vs))

instance Zonk AltCon where
  zonk (AltData d vs) = AltData d <$> zonk vs
  zonk v              = pure v

-- TODO: use Level?
data Alt = Alt
  { alt  :: AltCon
  , body :: Term
  }
  deriving (Show)

instance Pretty Alt where
  pretty alt = pretty alt.alt <+> "->" <> P.nest 2 (P.hardline <> pretty alt.body)

instance Zonk Alt where
  zonk (Alt a b) = Alt <$> zonk a <*> zonk b

newtype PrimOp
  = PrimOp Text
  deriving (Show)

instance Pretty PrimOp where
  pretty (PrimOp p) = "#" <> pretty p

data Term where
  -- | binding of values to variables
  TLetV :: Var -> Value -> Term -> Term
  -- | bindings of the result of prim-ops
  TLetP :: Var -> PrimOp -> Seq Var -> Term -> Term
  -- | declaration of continuations
  TLetK :: Label -> Seq Var -> Term -> Term -> Term
  -- | declaration of functions
  TLetF :: Var -> Label -> Seq Var -> Term -> Term -> Term
  -- | continuation application
  TJmp :: Label -> Seq Var -> Term
  -- | function application
  TApp :: Var -> Label -> Seq Var -> Term
  -- | case expressions
  TMatch :: Var -> Seq Alt -> Maybe Term -> Term
  -- | unreachable code
  TError :: Var -> Term
  deriving (Show)

(<++>) :: P.Doc ann -> P.Doc ann -> P.Doc ann
(<++>) a b = a <> P.softline <> b

instance Pretty Term where
  pretty (TLetV v x e) = "letv" <+> pretty v <+> "="
    <+> pretty x
    <> P.hardline <> "in" <+> pretty e
  pretty (TLetP v p xs e) = "letp" <+> pretty v <+> "="
    <+> P.concatWith (<+>) (pretty p :<| (P.pretty <$> xs))
    <> P.hardline <> "in" <+> pretty e
  pretty (TLetK (Label k _) xs e1 e2) = "letk"
    <+> P.concatWith (<+>) (pretty k :<| (P.pretty <$> xs)) <+> "="
    <> P.nest 2 (P.hardline <> pretty e1)
    <> P.hardline <> "in" <+> pretty e2
  pretty (TLetF (Var f _) (Label k _) xs e1 e2) = "letf" <> P.braces (pretty k)
    <+> P.concatWith (<++>) (pretty f :<| (P.pretty <$> xs)) <++> "="
    <> P.nest 2 (P.hardline <> pretty e1)
    <> P.hardline <> "in" <+> pretty e2
  pretty (TJmp (Label k _) xs) = "jmp"
    <+> P.concatWith (<+>) (pretty k :<| (P.pretty <$> xs))
  pretty (TApp (Var f _) (Label k _) xs) = "call" <> P.braces (pretty k)
    <+> P.concatWith (<+>) (pretty f :<| (P.pretty <$> xs))
  pretty (TMatch v alts fb) = P.nest 2 $ "match" <+> pretty v <+> P.braces (
      P.hardline <> "|" <+> P.concatWith (\x y -> x <> P.hardline <> "|" <+> y) alts'
      <> P.nest (-2) P.hardline
    )
    where
      alts' = case fb of
        Nothing -> P.pretty <$> alts
        Just a  -> (P.pretty <$> alts) :|> "__default__ -> " <> pretty a
  pretty (TError v) = "error" <+> pretty v

instance Zonk Term where
  zonk (TLetV v x e) = TLetV <$> zonk v <*> zonk x <*> zonk e
  zonk (TLetP v p xs e) = TLetP <$> zonk v <*> pure p <*> zonk xs <*> zonk e
  zonk (TLetK k xs e1 e2) = TLetK <$> zonk k <*> zonk xs <*> zonk e1 <*> zonk e2
  zonk (TLetF f k xs e1 e2) = TLetF <$> zonk f <*> zonk k <*> zonk xs <*> zonk e1 <*> zonk e2
  zonk (TJmp k xs) = TJmp <$> zonk k <*> zonk xs
  zonk (TApp f k xs) = TApp <$> zonk f <*> zonk k <*> zonk xs
  zonk (TMatch v alts fb) = TMatch <$> zonk v <*> zonk alts <*> zonk fb
  zonk v = pure v

data Decl where -- | top-level value declarations
  -- DValue :: Var -> Value -> Decl
  -- | top-level term declarations
  DTerm :: Var -> Label -> Seq Var -> Term -> Decl
  deriving (Show)

instance Pretty Decl where
  pretty (DTerm (Var f _) (Label k _) xs e) = "letf" <> P.braces (pretty k)
    <+> P.concatWith (<++>) (pretty f :<| (P.pretty <$> xs)) <+> "="
    <> P.nest 2 (P.hardline <> pretty e)

instance Zonk Decl where
  zonk (DTerm v k xs e) = DTerm <$> zonk v <*> zonk k <*> zonk xs <*> zonk e
