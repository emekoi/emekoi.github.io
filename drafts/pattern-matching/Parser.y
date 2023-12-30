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
import Lexer                      (unToken, unToken')
import Lexer                      qualified as L
import Prettyprinter              ((<+>))
import Prettyprinter              qualified as P
}

%name parse orPattern
%tokentype { L.Token }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.Token _ L.EOF }

%token
  variableT    { L.Token _ (L.Variable _) }
  constructorT { L.Token _ (L.Constructor _) }
  -- matchT       { L.Token _ L.Match }
  -- whenT        { L.Token _ L.When }
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

variable :: { (L.Range, Text) }
  : variableT { unToken $1 \r (L.Variable name) -> (r, decodeUtf8 name) }

constructor :: { (L.Range, Text) }
  : constructorT { unToken $1 \r (L.Constructor name) -> (r, decodeUtf8 name) }

orPattern :: { Pattern L.Range }
  : pattern               { $1 }
  | orPattern '|' pattern {
      case $1 of
        POr r ps -> POr (r <-> $3) (ps :|> $3)
        _ -> POr ($1 <-> $3) (Empty :|> $1 :|> $3)
    }

pattern :: { Pattern L.Range }
  : '(' orPattern ')'                       { $2 }
  | '_'                                     { unToken $1 \r _ -> PWild r }
  | variable                                { let (r, v) = $1 in PAs r v (PWild r) }
  | variable '@' pattern                    { let (r, v) = $1 in PAs (r <-> $3) v $3  }
  | constructor                             { let (r, c) = $1 in PData r c Empty }
  | constructor '(' sepBy(pattern, ',') ')' { let (r, c) = $1 in PData (r <-> $3 <-> $4) c $3 }

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

data Pattern a
  = PWild a
  | PAs a Text (Pattern a)
  | POr a (Seq (Pattern a))
  | PData a Text (Seq (Pattern a))
  deriving (Show, Foldable)

instance (Foldable f) => HasRange (f L.Range) where
  range = foldMap' id

instance (Foldable f, Foldable g, HasRange (f L.Range)) => HasRange (g (f L.Range)) where
  range = range . foldMap range

instance P.Pretty (Pattern l) where
  pretty = go False
    where
      go _ (PWild _)           = "_"
      go _ (PAs _ x (PWild _)) = P.pretty x
      go _ (PAs _ x p)         = P.pretty x <> "@" <> go True p
      go _ (PData _ c Empty)   = P.pretty c
      go False (PData _ c ps)  = P.pretty c <> P.parens (P.concatWith (\x y -> x <> "," <+> y) (go False <$> ps))
      go False (POr _ ps)      = P.concatWith (\x y -> x <+> "|" <+> y) (go False <$> ps)
      go True x                = P.parens (go False x)
}
