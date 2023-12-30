{
module Parser (parse, Pattern(..), HasRange(..)) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Foldable              (foldMap')
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Maybe                 (fromJust)
import Data.Monoid                (First (..))
import Data.Sequence              (Seq (..))
import Data.Sequence              qualified as Seq
import Data.Text                  (Text)
import Data.Text.Lazy             qualified as LT
import Data.Text.Lazy.Encoding    qualified as LE
import Lexer                      (unToken)
import Lexer                      qualified as L
import Prettyprinter              ((<+>))
import Prettyprinter              qualified as P
}

%name parse pattern_
%tokentype { L.Token }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.Token _ L.EOF }

%token
  variableT    { L.Token _ (L.Variable _) }
  constructorT { L.Token _ (L.Constructor _) }
  intT         { L.Token _ (L.Int _) }
  stringT      { L.Token _ (L.String _) }
  -- matchT       { L.Token _ L.Match }
  letT         { L.Token _ L.Let }
  inT          { L.Token _ L.In }
  -- '{'          { L.Token _ L.LBracket }
  -- '}'          { L.Token _ L.RBracket }
  '('          { L.Token _ L.LParen }
  ')'          { L.Token _ L.RParen }
  -- '->'         { L.Token _ L.RArrow }
  -- '<-'         { L.Token _ L.LArrow }
  ','          { L.Token _ L.Comma }
  '|'          { L.Token _ L.Pipe }
  '@'          { L.Token _ L.At }
  '_'          { L.Token _ L.Underscore }
  '='          { L.Token _ L.Eq }

-- %right inT

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

pattern :: { Pattern L.Range }
  : pattern_             { $1 }
  | pattern '|' pattern_ {
      case $1 of
        POr r ps -> POr (r <-> $3) (ps :|> $3)
        _ -> POr ($1 <-> $3) (Empty :|> $1 :|> $3)
    }

pattern_ :: { Pattern L.Range }
  : '(' pattern ')'                          { $2 }
  | '_'                                      { unToken $1 \r _ -> PWild r }
  | variable                                 { let (Name r _) = $1 in PAs r $1 (PWild r) }
  | variable '@' pattern_                    { PAs ($1 <-> $3) $1 $3  }
  -- NOTE: when parsing `many(pattern_)` we get a SR conflict because
  -- C (<pattern>) and C(<pattern>) look the same
  | constructor                              { let (Name r c) = $1 in PData r c Empty }
  | constructor '(' sepBy1(pattern, ',') ')'  { let (Name r c) = $1 in PData (r <-> $3 <-> $4) c $3 }

-- row :: { (Pattern L.Range, Maybe (Pattern L.Range, Name L.Range)) }
--   : pattern { ($1, Nothing) }
--   | pattern '{' pattern '<-' variable '}' { ($1, Just ($3, $5)) }

expr :: { Expr L.Range }
  : variable                      { EVar (range $1) $1 }
  | intT                          { unToken $1 \r (L.Int i) -> EInt r i }
  | stringT                       { unToken $1 \r (L.String s) -> EString r s }
  | expr '(' sepBy(expr, ',') ')' { EApp ($1 <-> $4) $1 $3 }
  -- TODO: is this right?
  | decl inT expr %shift          { ELet ($1 <-> $3) $1 $3 }

decl :: { Decl L.Range }
  : letT variable many(pattern) '=' expr { Decl ($1 <-> $5) $2 $3 $5 }

decls :: { Seq (Decl L.Range) }
  : many(decl) { $1 }

{
decodeUtf8 :: ByteString -> Text
decodeUtf8 = LT.toStrict . LE.decodeUtf8

parseError :: L.Token -> L.Alex a
parseError _ = do
  (p, _, _, _) <- L.alexGetInput
  L.alexError' p "parse error"

lexer :: (L.Token -> L.Alex a) -> L.Alex a
lexer = (L.alexMonadScan' >>=)

class HasRange r where
  range :: r -> L.Range

(<->) :: (HasRange r, HasRange r') => r -> r' -> L.Range
(range -> a) <-> (range -> b) = a <> b

instance HasRange L.Range where
  range = id

instance HasRange L.Token where
  range (L.Token r _) = r

instance (Foldable f) => HasRange (f L.Range) where
  range = foldMap' id

instance (Foldable f, Foldable g, HasRange (f L.Range)) => HasRange (g (f L.Range)) where
  range = range . foldMap range

data Name a
  = Name a Text
  deriving (Show, Foldable, Functor)

data Pattern a
  = PWild a
  | PAs a (Name a) (Pattern a)
  | POr a (Seq (Pattern a))
  | PData a Text (Seq (Pattern a))
  deriving (Show, Foldable, Functor)

instance P.Pretty (Name a) where
  pretty (Name _ n) = P.pretty n

instance P.Pretty (Pattern a) where
  pretty = go False
    where
      go _ (PWild _)           = "_"
      go _ (PAs _ x (PWild _)) = P.pretty x
      go _ (PAs _ x p)         = P.pretty x <> "@" <> go True p
      go _ (PData _ c Empty)   = P.pretty c
      go False (PData _ c ps)  = P.pretty c <>
        P.parens (P.concatWith (\x y -> x <> "," <+> y) (go False <$> ps))
      go False (POr _ ps)      =
        P.concatWith (\x y -> x <+> "|" <+> y) (go False <$> ps)
      go True x                = P.parens (go False x)

data Decl a
  = Decl a (Name a) (Seq (Pattern a)) (Expr a)
  deriving (Foldable, Show, Functor)

instance P.Pretty (Decl a) where
  pretty (Decl _ n Empty e) = "let" <+> P.pretty n <+> "=" <+> P.pretty e
  pretty (Decl _ n xs e) = "let" <+> P.pretty n
    <+> P.concatWith (<+>) (P.pretty <$> xs) <+> "=" <+> P.pretty e

data Expr a
  = EInt a Integer
  | EVar a (Name a)
  | EString a ByteString
  | EApp a (Expr a) (Seq (Expr a))
  | ELet a (Decl a) (Expr a)
  deriving (Foldable, Show, Functor)

instance P.Pretty (Expr a) where
  pretty = go False
    where
      go True x                = P.parens (go False x)
      go _ (EInt _ i)          = P.pretty i
      go _ (EVar _ v)          = P.pretty v
      go _ (EString _ s)       = P.pretty $ LE.decodeUtf8 s
      go _ (EApp _ f xs)       = P.pretty f <>
        P.parens (P.concatWith (\x y -> x <> "," <+> y) (go False <$> xs))
      go _ (ELet _ d e)        = P.pretty d <+> "in" <+> (go True e)
}
