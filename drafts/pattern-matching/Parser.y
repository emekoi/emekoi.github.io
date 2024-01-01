{
{-# LANGUAGE DefaultSignatures #-}

module Parser
  ( HasRange(..)
  , L.Range(..)
  , Alt(..)
  , DataCon(..)
  , Decl(..)
  , Expr(..)
  , ExprDecl(..)
  , Module(..)
  , Name(..)
  , Pattern(..)
  , (<->)
  , parse
  , r2p
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

%%

optional(p)
  :   { Nothing }
  | p { Just $1 }

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
        POr r ps -> POr (r <-> $3) (ps :|> $3)
        _ -> POr ($1 <-> $3) (Empty :|> $1 :|> $3)
    }

alt :: { Alt L.Range }
  : pattern_3 '->' expr                             { Alt ($1 <-> $3) $1 Nothing $3 }
  | pattern_3 '{' pattern_3 '<-' expr '}' '->' expr { Alt ($1 <-> $8) $1 (Just ($3, $5)) $8 }

atom :: { Expr L.Range }
  : variable     { EVar (range $1) $1 }
  | intT         { unToken $1 \r (L.Int i) -> EInt r i }
  | stringT      { unToken $1 \r (L.String s) -> EString r (decodeUtf8 s) }
  | constructor  { EData (range $1) $1 }
  | '(' expr ')' { $2 }

expr :: { Expr L.Range }
  : atom                   { $1 }
  | atom some(atom)        { EApp (rangeSeq $1 $2) $1 $2 }
  | decl inT expr          { ELet ($1 <-> $3) $1 $3 }
  | matchT expr '{'
    optional('|') sepBy(alt, '|')
    '}'                    { EMatch ($1 <-> $6) $2 $5 }

dataCon_1 :: { DataCon L.Range }
  : constructor       { DataCon (range $1) $1 Empty }
  | '(' dataCon_2 ')' { $2 }

dataCon_2 :: { DataCon L.Range }
  : constructor many(dataCon_1) { DataCon (rangeSeq $1 $2) $1 $2 }

edecl :: { ExprDecl L.Range }
  : letT variable many(pattern_1) '=' expr { ExprDecl ($1 <-> $5) $2 $3 $5 }

decl :: { Decl L.Range }
  : edecl                                       { DExpr $1 }
  | dataT constructor '=' sepBy(dataCon_2, '|') { DData (rangeSeq $1 $4) $2 $4 }

decls :: { Module }
  : many(decl) { Module $1 }

{ {-# LINE 151 "Parser.y" #-}
decodeUtf8 :: ByteString -> Text
decodeUtf8 = LT.toStrict . LE.decodeUtf8

parseError :: L.Token -> L.Alex a
parseError (L.Token r t) = do
  p <- L.r2p' r
  throwError "parse error" [
    (p, This . ErrDoc $ P.hsep ["unexpected token:", P.viaShow t])
   ]

lexer :: (L.Token -> L.Alex a) -> L.Alex a
lexer = (L.alexMonadScan >>=)

class HasRange r where
  range :: r -> L.Range

  default range :: (Foldable f, r ~ f L.Range) => r -> L.Range
  range = fromJust . getFirst . foldMap pure

(<->) :: (HasRange r, HasRange r') => r -> r' -> L.Range
(range -> a) <-> (range -> b) = a <> b

r2p :: (HasRange r) => FilePath -> r -> Position
r2p fp r = L.r2p fp (range r)

instance HasRange L.Range where
  range = id

instance HasRange L.Token where
  range (L.Token r _) = r

rangeSeq :: (HasRange x, HasRange y) => x -> Seq y -> L.Range
rangeSeq x Empty     = range x
rangeSeq x (_ :|> y) = range x <> range y

data Name a
  = Name a Text
  deriving (Show, Foldable, Functor)

instance HasRange (Name L.Range) where
  range (Name r _) = r

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
  | POr a (Seq (Pattern a))
  deriving (Show, Foldable, Functor)

instance HasRange (Pattern L.Range)

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
      go False (POr _ ps)      =
        P.concatWith (\x y -> x <+> "|" <+> y) (go False <$> ps)
      go True x                = P.parens (go False x)

data DataCon a
 = DataCon a (Constructor a) (Seq (DataCon a))
  deriving (Foldable, Show, Functor)

instance HasRange (DataCon L.Range)

instance P.Pretty (DataCon a) where
  pretty = go False
    where
      go False (DataCon _ c xs) = P.pretty c <+>
        P.concatWith (<+>) (go True <$> xs)
      go True d@(DataCon _ _ xs) =
        let doc = go False d in
          if null xs then doc else doc

data ExprDecl a
  = ExprDecl a (Variable a) (Seq (Pattern a)) (Expr a)
  deriving (Foldable, Show, Functor)

instance HasRange (ExprDecl L.Range)

instance P.Pretty (ExprDecl a) where
  pretty (ExprDecl _ n (fmap P.pretty -> xs) e) =
    "let" <+> P.concatWith (<+>) (P.pretty n :<| xs) <+> "=" <+> P.pretty e

data Decl a
  = DExpr (ExprDecl a)
  | DData a (Constructor a) (Seq (DataCon a))
  deriving (Foldable, Show, Functor)

instance HasRange (Decl L.Range)

instance P.Pretty (Decl a) where
  pretty (DExpr e) = P.pretty e
  pretty (DData _ c (fmap P.pretty -> xs)) =
    "data" <+> P.pretty c <+> "="
      <+> P.concatWith (\x y -> x <+> "|" <+> y) xs

data Alt a
  = Alt a (Pattern a) (Maybe (Pattern a, Expr a)) (Expr a)
  deriving (Foldable, Show, Functor)

instance HasRange (Alt L.Range)

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
  | EData a (Constructor a)
  | EString a Text
  | EMatch a (Expr a) (Seq (Alt a))
  | EApp a (Expr a) (Seq (Expr a))
  | ELet a (Decl a) (Expr a)
  deriving (Foldable, Show, Functor)

instance HasRange (Expr L.Range)

instance P.Pretty (Expr a) where
  pretty = go False
    where
      go _ (EInt _ i)        = P.pretty i
      go _ (EVar _ v)        = P.pretty v
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
