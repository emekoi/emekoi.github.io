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
import Prettyprinter          qualified as P
import Util

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

newtype Trivial r
  = Trivial r
  deriving (Functor)

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

instance P.Pretty Name where
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

instance P.Pretty Type where
  pretty = go False
    where
      go _ (TyMeta i _)         = "?" <> P.pretty i
      go _ (TyData (TypeId c))  = P.pretty c
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

tyZonk :: (MonadIO m) => Type -> m Type
tyZonk t@(TyMeta _ (Trivial m)) =
  readIORef m >>= \case
    Just m -> tyZonk m
    _ -> pure t
tyZonk (TyFun a b)  = TyFun <$> traverse tyZonk a <*> tyZonk b
tyZonk x            = pure x

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

data Label = MkLabel Name (TrivialEq (Seq Type))
  deriving (Eq, Ord)

pattern Label :: Name -> Seq Type -> Label
pattern Label n args = MkLabel n (TrivialEq args)
{-# COMPLETE Label :: Label #-}

instance Show Label where
  show (Label v _) = show v

data Value where
  -- | int values
  VInt :: Integer -> Value
  -- | string values
  VString :: Text -> Value
  -- | tagged union from algebraic data types
  VData :: DataId -> Seq Var -> Value
  deriving (Eq, Show)

data AltCon
  = AltData DataId (TrivialEq (Seq Var))
  | AltString Text
  | AltInt Integer
  deriving (Eq, Ord, Show)

-- TODO: use Level?
data Alt = Alt
  { alt  :: AltCon
  , body :: Term
  }
  deriving (Eq, Show)

newtype PrimOp
  = PrimOp Text
  deriving (Eq, Show)

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
  TCase :: Var -> Seq Alt -> Maybe Term -> Term
  -- | unreachable code
  TError :: Text -> Term
  deriving (Eq, Show)

data Decl where -- | top-level value declarations
  -- DValue :: Var -> Value -> Decl
  -- | top-level term declarations
  DTerm :: Label -> Label -> Term -> Decl
  deriving (Eq, Show)

