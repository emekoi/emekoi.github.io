{
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLists #-}

module Parser
  ( HasRange
  , HasInfo(..)
  , L.Range(..)
  , Alt(..)
  , DataCon(..)
  , Decl(..)
  , Expr(..)
  , ExprDecl(..)
  , Module(..)
  , Name(Name, getName)
  , Pattern(..)
  , Constructor
  , Variable
  , Type
  , (<->)
  , parse
  , r2p
  , range
  , rangeSeq
) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Maybe                 (fromJust)
import Data.Monoid                (First (..))
import Data.Sequence              (Seq (..))
import Data.Sequence              qualified as Seq
import Data.Text                  (Text)
import Data.Text.Lazy             qualified as LT
import Data.Text.Lazy.Encoding    qualified as LE
import Error
import Lexer                      (unToken)
import Lexer                      qualified as L
import Prettyprinter              ((<+>))
import Prettyprinter              qualified as P
}

%name parse decls
%tokentype { L.Token }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.Token _ L.EOF }
%expect 0

%token
  variableT    { L.Token _ (L.Variable _) }
  constructorT { L.Token _ (L.Constructor _) }
  intT         { L.Token _ (L.Int _) }
  stringT      { L.Token _ (L.String _) }
  dataT        { L.Token _ L.Data }
  inT          { L.Token _ L.In }
  letT         { L.Token _ L.Let }
  matchT       { L.Token _ L.Match }
  '{'          { L.Token _ L.LBracket }
  '}'          { L.Token _ L.RBracket }
  '('          { L.Token _ L.LParen }
  ')'          { L.Token _ L.RParen }
  '->'         { L.Token _ L.RArrow }
  '<-'         { L.Token _ L.LArrow }
  '|'          { L.Token _ L.Pipe }
  '@'          { L.Token _ L.At }
  '_'          { L.Token _ L.Underscore }
  '='          { L.Token _ L.Eq }
  ':'          { L.Token _ L.Colon }
  '#'          { L.Token _ L.Hash }

%%

optional(p)
  :   { Nothing }
  | p { Just $1 }

optionalB(p)
  :   { False }
  | p { True }

many(p)
  :           { Empty }
  | many(p) p { $1 :|> $2 }

some(p)
  : p many(p) { $1 :<| $2 }

sepBy1(p, sep)
  : p                    { Seq.singleton $1 }
  | sepBy1(p, sep) sep p { $1 :|> $3 }

sepBy(p, sep)
  :                { Empty }
  | sepBy1(p, sep) { $1 }

variable :: { Name L.Range }
  : variableT { unToken $1 \r (L.Variable name) -> Name r (decodeUtf8 name) }

constructor :: { Name L.Range }
  : constructorT { unToken $1 \r (L.Constructor name) -> Name r (decodeUtf8 name) }

pattern_1 :: { Pattern L.Range }
  : '(' pattern_3 ')'                 { $2 }
  | '(' pattern_3 ':' constructor ')' { PType ($1 <-> $5) $2 $4 }
  | '_'                               { unToken $1 \r _ -> PWild r }
  | intT                              { unToken $1 \r (L.Int i) -> PInt r i }
  | stringT                           { unToken $1 \r (L.String s) -> PString r (decodeUtf8 s) }
  | variable                          { let (Name r _) = $1 in PAs r $1 (PWild r) }
  | constructor                       { PData (range $1) $1 Empty }
  | variable '@' pattern_1            { PAs ($1 <-> $3) $1 $3  }

pattern_2 :: { Pattern L.Range }
  : pattern_1                   { $1 }
  | constructor some(pattern_1) { PData (rangeSeq $1 $2) $1 $2 }

pattern_3 :: { Pattern L.Range }
  : pattern_2               { $1 }
  | pattern_3 '|' pattern_2 {
      case $1 of
        POr r p ps -> POr (r <-> $3) p (ps :|> $3)
        _ -> POr ($1 <-> $3) $1 (Seq.singleton $3)
    }

alt :: { Alt L.Range }
  : pattern_3 '->' expr                             { Alt ($1 <-> $3) $1 Nothing $3 }
  | pattern_3 '{' pattern_3 '<-' expr '}' '->' expr { Alt ($1 <-> $8) $1 (Just ($3, $5)) $8 }

atom :: { Expr L.Range }
  : variable     { EVar (range $1) $1 }
  | variable '#' { EPrim ($1 <-> $2) $1 }
  | intT         { unToken $1 \r (L.Int i) -> EInt r i }
  | stringT      { unToken $1 \r (L.String s) -> EString r (decodeUtf8 s) }
  | constructor  { EData (range $1) $1 }
  | '(' expr ')' { $2 }

expr :: { Expr L.Range }
  : atom                   { $1 }
  | atom some(atom)        { EApp (rangeSeq $1 $2) $1 $2 }
  | letT edecl inT expr    { ELet ($1 <-> $4) $2 $4 }
  | matchT expr '{'
    optional('|') sepBy(alt, '|')
    '}'                    { EMatch ($1 <-> $6) $2 $5 }

dataCon :: { DataCon L.Range }
  : constructor many(constructor) { DataCon (rangeSeq $1 $2) $1 $2 }

edecl :: { ExprDecl L.Range }
  : variable many(pattern_1) '=' expr { ExprDecl ($1 <-> $4) $1 $2 $4 }

decl :: { Decl L.Range }
  : letT edecl                                               { DExpr $2 }
  | dataT optionalB('#') constructor                         { DData ($1 <-> $3) $3 $2 Empty }
  | dataT optionalB('#') constructor '=' sepBy(dataCon, '|') { DData (rangeSeq $1 $5) $3 $2 $5 }

decls :: { Module }
  : many(decl) { Module $1 }

{ {-# LINE 163 "Parser.y" #-}
decodeUtf8 :: ByteString -> Text
decodeUtf8 = LT.toStrict . LE.decodeUtf8

parseError :: L.Token -> L.Alex a
parseError (L.Token r t) = do
  p <- L.r2p' r
  throwError "parse error" [
    (p, This ["unexpected token:", ErrShow t ])
   ]

lexer :: (L.Token -> L.Alex a) -> L.Alex a
lexer = (L.alexMonadScan >>=)

class HasInfo r i | r -> i where
  info :: r -> i

  default info :: (Foldable f, r ~ f i) => r -> i
  info = fromJust . getFirst . foldMap pure

type HasRange r = HasInfo r L.Range

range :: (HasInfo r L.Range) => r -> L.Range
range = info

(<->) :: (HasRange r, HasRange r') => r -> r' -> L.Range
(range -> a) <-> (range -> b) = a <> b

r2p :: (HasRange r) => FilePath -> r -> Position
r2p fp r = L.r2p fp (range r)

instance HasInfo L.Range L.Range where
  info = id

instance HasInfo L.Token L.Range where
  info (L.Token r _) = r

rangeSeq :: (HasRange x, HasRange y) => x -> Seq y -> L.Range
rangeSeq x Empty     = range x
rangeSeq x (_ :|> y) = range x <> range y

data Name a
  = Name { nameInfo :: a, getName :: Text }
  deriving (Foldable, Functor, Traversable)

instance Show (Name a) where
  show (Name _ n) = show n

instance Eq (Name a) where
  x == y = getName x == getName y

instance Ord (Name a) where
  x `compare` y = compare (getName x) (getName y)

instance HasInfo (Name i) i

instance P.Pretty (Name a) where
  pretty (Name _ n) = P.pretty n

type Variable = Name
type Constructor = Name
type Type = Name

data Pattern a
  = PWild a
  | PInt a Integer
  | PString a Text
  | PAs a (Name a) (Pattern a)
  | PType a (Pattern a) (Type a)
  | PData a (Constructor a) (Seq (Pattern a))
  | POr a (Pattern a) (Seq (Pattern a))
  deriving (Show, Foldable, Functor)

instance HasInfo (Pattern i) i

instance P.Pretty (Pattern a) where
  pretty = go False
    where
      go _ (PWild _)           = "_"
      go _ (PInt _ i)          = P.pretty i
      go _ (PString _ s)       = P.pretty s
      go _ (PAs _ x (PWild _)) = P.pretty x
      go _ (PAs _ x p)         = P.pretty x <> "@" <> go True p
      go _ (PType _ p t)       = P.parens $ go False p <+> ":" <+> P.pretty t
      go _ (PData _ c Empty)   = P.pretty c
      go False (PData _ c ps)  =
        P.concatWith (<+>) (P.pretty c :<| (go True <$> ps))
      go False (POr _ p ps)    =
        P.concatWith (\x y -> x <+> "|" <+> y) (go False <$> (p :<| ps))
      go True x                = P.parens (go False x)

data DataCon a
 = DataCon a (Constructor a) (Seq (Type a))
  deriving (Foldable, Show, Functor)

instance HasInfo (DataCon i) i

instance P.Pretty (DataCon a) where
  pretty (DataCon _ c xs) = P.concatWith (<+>) (P.pretty <$> (c :<| xs))

data ExprDecl a
  = ExprDecl a (Name a) (Seq (Pattern a)) (Expr a)
  deriving (Foldable, Show, Functor)

instance HasInfo (ExprDecl i) i

instance P.Pretty (ExprDecl a) where
  pretty (ExprDecl _ n xs e) =
    "let" <+> P.concatWith (<+>) (P.pretty n :<| (wrap <$> xs)) <+> "=" <+> P.pretty e
    where
      wrap p@(PData _ _ ps) | not (null ps) = P.parens $ P.pretty p
      wrap p@(POr {}) = P.parens $ P.pretty p
      wrap p = P.pretty p

data Decl a
  = DExpr (ExprDecl a)
  | DData a (Name a) Bool (Seq (DataCon a))
  deriving (Foldable, Show, Functor)

instance HasInfo (Decl i) i

instance P.Pretty (Decl a) where
  pretty (DExpr e) = P.pretty e
  pretty (DData _ c m (fmap P.pretty -> xs)) =
    if null xs then d <+> P.pretty c else
      d <+> P.pretty c <+> "="
        <+> P.concatWith (\x y -> x <+> "|" <+> y) xs
    where
      d = if m then "data#" else "data"

data Alt a
  = Alt a (Pattern a) (Maybe (Pattern a, Expr a)) (Expr a)
  deriving (Foldable, Show, Functor)

instance HasInfo (Alt i) i

instance P.Pretty (Alt a) where
  pretty (Alt _ p Nothing e) =
    P.pretty p <+> "->" <+> P.pretty e
  pretty (Alt _ p (Just (p', e')) e) =
    P.pretty p
      <+> P.braces (P.pretty p' <+> "<-" <+> P.pretty e')
      <+> "->"
      <+> P.pretty e

data Expr a
  = EInt a Integer
  | EVar a (Variable a)
  | EPrim a (Name a)
  | EData a (Constructor a)
  | EString a Text
  | EMatch a (Expr a) (Seq (Alt a))
  | EApp a (Expr a) (Seq (Expr a))
  | ELet a (ExprDecl a) (Expr a)
  deriving (Foldable, Show, Functor)

instance HasInfo (Expr i) i

instance P.Pretty (Expr a) where
  pretty = go False
    where
      go _ (EInt _ i)        = P.pretty i
      go _ (EVar _ v)        = P.pretty v
      go _ (EPrim _ p)       = P.pretty p <> "#"
      go _ (EData _ c)       = P.pretty c
      go _ (EString _ s)     = P.pretty s
      go _ (EMatch _ e xs)   = "match" <+> P.pretty e <+>
        P.braces (P.concatWith (\x y -> x <+> "|" <+> y) (P.pretty <$> xs))
      go False (EApp _ f xs) = P.pretty f <+>
        P.concatWith (<+>) (go True <$> xs)
      go False (ELet _ d e)  = P.pretty d <+> "in" <+> (go True e)
      go True x              = P.parens (go False x)

data Module = Module (Seq (Decl L.Range))
  deriving (Show)

instance P.Pretty Module where
  pretty (Module ds) =
    P.concatWith (\x y -> x <> P.line <> y) (P.pretty <$> ds)
}
