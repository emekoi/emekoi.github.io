---
title: Pattern Matching for Great Good
published: 2023-11-30
tags: [pl,haskell]
packages: [microlens,microlens-ghc,containers,text,prettyprinter,base]
slug: pattern-matching
link-citations: true
csl: bib/ieee.csl
---

<details>
  <summary>Haskell language extensions and module imports.</summary>

``` haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE StandaloneDeriving    #-}

module PatternMatching (module PatternMatching) where

import Control.Monad.Trans.Reader
import Control.Monad
import Data.Foldable              (foldl')
import Data.Functor               (($>))
import Data.List                  qualified as List
import Data.Map.Strict            (Map)
import Data.Map.Strict            qualified as Map
import Data.Sequence              (Seq (..))
import Data.Sequence              qualified as Seq
import Data.Set                   (Set)
import Data.Set                   qualified as Set
import Data.Text                  (Text)
import Lens.Micro
import Prettyprinter              ((<+>))
import Prettyprinter              qualified as P
```
</details>

# Literate Haskell
This post is a literate Haskell file written in Markdown. Since it's written in Markdown it's a little more work to open it in GHCI than regular literate Haskell, but not by a massive amount:

``` shell
curl -sL 'https://${TODO: post url}' -o <file>.lhs
cabal install -j --lib --package-env . \
  "$(sed -n 's/^packages:\s*\[\(.*\)\]$/\1/p' <file>.lhs | tr ',' ' ')"
cabal install -j markdown-unlit
ghci -pgmL markdown-unlit <file>.lhs
```

# Introduction
Among the many features that set functional programming languges apart from other


First and foremost, we need things to pattern match on before we can start talking about pattern matching algorithms. In the spirit of keeping things relatively simple, our "language" will only have <abbr title="algebraic data types">ADTs</abbr>:

``` haskell
newtype TypeInfo
  = TypeInfo { span :: Maybe Word }
  deriving (Eq, Ord)

data DataCon = DataCon
  { name  :: Text
  , arity :: Int
  , info  :: TypeInfo
  }
  deriving (Eq, Ord)
```

In a "real" language you might want data types such as integers, floats, or chars, but for the purposes of demonstration we can model them as ADTs since the logic will be exactly the same:

``` haskell
pattern Nat :: DataCon
pattern Nat = DataCon "Nat" 0 (TypeInfo Nothing)

pattern Char :: DataCon
pattern Char = DataCon "Char" 0 (TypeInfo (Just 255))
```

Now that we have data constructors, we need a grammar of patterns that we can use to write match expressions. Loosely modeling our pattern syntax after a mix of Haskell and OCaml we arrive at an EBNF grammar something like this:

``` ebnf
<identifier> ::= [a-z] ([0-9] | [a-z])+

<constructor> ::= [A-Z] <identifier>

<pattern> ::= "(" <pattern> ")"
            | <identifier> "@" <pattern>
            | <constructor> <pattern>*
            | <pattern> "|" <pattern>
            | _;
```

If you look closely you might notice that our grammar does not account for pattern variables like you see in Haskell or OCaml. This is because, we can express pattern variables as `as` patterns on top of `_` patterns, e.g `Foo a b c` becomes `Foo a@_ b@_ c@_`. However, if you look even closer at this grammar you'll also notice that it allows some patterns that are perfectly valid to write, but you would never want to write in practice such as `_ | p1 | ... | pn`.
In @marangetCompilingPatternMatching2008, Maranget handles this by defining what he calls *generalized contrstructor patterns* which restrict the places in which wildcards can appear:

``` ebnf
<gcpattern> ::= <constructor> <pattern>*
              | <gcpattern> "|" <pattern>;
```

> TODO: mention preprocessing of patterns in to GCPs

Generalized constructor patterns form a strict subset of full pattern syntax we defined above and simplify the later parts of the pattern matching algorithm.

> TODO: mention how there are no top level as patterns in GCPs

However, they don't disallow patterns such as `a@b@c@p` or `p1 | _ | p2`, so we make a slight adjustment to grammar of generalized contrstructor patterns to arrive at the following:

``` ebnf
identifier ::= (* omitted *)

constructor ::= (* omitted *)

pattern-0 ::= <constructor> { <pattern-2> }

pattern-1 ::= <pattern-0> { '|' <pattern-0> } [ <pattern-2> ]

pattern-2 ::= [ <identifier> '@' ] <pattern-3>

pattern-3 ::= '(' <pattern-3> ')'
            | <patttern-1>
            | _
```

> TODO: fix this grammar description

``` ebnf
<identifier> ::= [a-z] ([0-9] | [a-z] | [A-Z])*
<constructor> ::= [A-Z] <identifier>?
<pattern_1> ::= (<identifier> "@")? ("_" | <constructor> | "(" <pattern_2> ")")
<pattern_2> ::= (<pattern_3> " | ")* <pattern_1>
<pattern_3> ::= <constructor> (" " <pattern_1>)*
<pattern> ::= <pattern_1> | <pattern_2>
```

> Whether you actually need this much precision in the grammar of your patterns is somewhat up for debate.

Naively translating this to Haskell we would get two mutually recursive ADTs with very similar constructors.

``` {.haskell .ignore}
data HighPattern where
  HighWild :: HighPattern
  HighAs :: Text -> HighPattern -> HighPattern
  HighData :: DataCon -> Seq HighPattern -> HighPattern
  HighOr :: Seq LowPattern -> HighPattern -> HighPattern

data LowPattern where
  LowData :: DataCon -> Seq HighPattern -> LowPattern
  LowOr :: Seq LowPattern -> HighPattern -> LowPattern

lowerPattern :: HighPattern -> (Maybe LowPattern, Maybe Text)
lowerPattern HighWild        = (Nothing, Nothing)
lowerPattern (HighAs x p)    = lowerPattern p $> Just x
lowerPattern (HighData c ps) = (Just (LowData c ps), Nothing)
lowerPattern (HighOr qs p)   = (Just (LowOr qs p), Nothing)

raisePattern :: LowPattern -> HighPattern
lowerPattern (LowData c ps) = HighData c ps
lowerPattern (LowOr qs p)   = HighOr qs p
```

Instead we can combine both ADTs into one using a GADT and a type level tag. Furthermore, remember when I said that we can

``` haskell
data Level
  = Low
  | High

data NESeq x = NESeq x (Seq x)
  deriving (Functor, Foldable)

data Pattern (l :: Level) where
  PWild :: Pattern 'High
  PVar :: Text -> Pattern 'High
  PAs :: Text -> Pattern 'Low -> Pattern 'High
  PData :: DataCon -> Seq (Pattern 'High) -> Pattern l
  POr :: NESeq (Pattern 'Low) -> Pattern 'High -> Pattern l
```

This is probably the most advanced use of Haskell in this post, and while it is not strictly necessary, I find that using GADTs simplifies and the code and reduces repitition.

It is also useful to define a helper function to convert, `High` patterns to `Low` patterns while caputuring any aliases introduced by `as` patterns

``` haskell
lower :: Pattern 'High -> (Maybe (Pattern 'Low), Maybe Text)
lower PWild        = (Nothing, Nothing)
lower (PVar x)     = (Nothing, Just x)
lower (PAs x p)    = (Just p, Just x)
lower (PData c ps) = (Just (PData c ps), Nothing)
lower (POr qs p)   = (Just (POr qs p), Nothing)
```

# Matrix Decomposition

Now that we have pattern language defined we can proceed into the core of the pattern matching algoritm definined in @marangetCompilingPatternMatching2008. The core idea is the manipulation of pattern matrices

$$
P = \begin{pmatrix}
1 & 2 & 3\\
a & b & c
\end{pmatrix}
$$

# Show me the Code!

``` haskell
newtype Var
  = Var Int
  deriving (Enum, Eq, Ord)

type MatchM = Reader Int

shift :: Int -> MatchM r -> MatchM r
shift n = local (+ n)

runMatchCompile :: MatchM r -> r
runMatchCompile m = runReader m 0

matchConVars :: DataCon -> MatchM (Seq Var)
matchConVars (DataCon _ span _) = do
  currentVar <- ask
  pure $ Seq.fromFunction span (\i -> Var (currentVar + i))

data Alt r = Alt
  { constructor :: DataCon
  , boundVars   :: (Seq Var)
  , altRhs      :: (Match r)
  }

data Match r
  = Switch
  { scrutinee :: Var
  , arms      :: (Seq (Alt r))
  , fallback  :: (Maybe (Match r))
  }
  | Leaf
  { match :: r
  }
  | Fail

data MatchRow r = Row
  { cols  :: Map Var (Pattern 'Low)
  , guard :: Maybe (Var, Pattern 'Low)
  , vars  :: Map Text Var
  , body  :: r
  }

rColsL :: Lens' (MatchRow r) (Map Var (Pattern 'Low))
rColsL f x = (\x' -> x {cols = x'}) <$> f x.cols

rVarsL :: Lens' (MatchRow r) (Map Text Var)
rVarsL f x = (\x' -> x {vars = x'}) <$> f x.vars

rowMatchVar :: Var -> Pattern 'Low -> MatchRow r -> MatchRow r
rowMatchVar x p = rColsL %~ Map.insert x p

rowBindVar :: Text -> Var -> MatchRow r -> MatchRow r
rowBindVar x v = rVarsL %~ Map.insert x v

newtype PatternMatrix r
  = Matrix { rows :: Seq (MatchRow r) }

-- | convert a `Pattern 'High` to `Pattern 'Low` making sure to
-- add any variables introduced by as patterns to the row
handlePattern
  :: Var
  -> Pattern 'High
  -> MatchRow r
  -> (MatchRow r -> b)
  -> b
handlePattern var pat row f = do
  let (pat', bound) = lower pat
      row'          = maybe row (\x -> rowBindVar x var row) bound
    in f $ case pat' of
      Nothing   -> row' & rColsL %~ Map.delete var
      Just pat' -> rowMatchVar var pat' row'
```

``` haskell
-- | expand the or pattern into multiple rows
expandOr
  :: (Foldable t)
  => Var
  -> t (Pattern 'Low)
  -> Pattern 'High
  -> MatchRow r
  -> (b -> MatchRow r -> b)
  -> b
  -> b
expandOr var qs p row f acc =
  foldl' (\acc q -> f acc $ rowMatchVar var q row) acc qs
    & handlePattern var p row . f

rowDefault :: Var -> PatternMatrix r -> PatternMatrix r
rowDefault var (Matrix rows) = Matrix $ foldl' f Empty rows
  where
    f acc row = case Map.lookup var row.cols of
      -- add the row as is
      Nothing -> acc :|> row
      -- expand the or pattern into multiple rows
      Just (POr qs p) -> expandOr var qs p row f acc
      -- delete rows with constructor patterns
      Just (PData {}) -> acc
```

``` haskell
rowSpecialize :: Var -> DataCon -> Seq Var -> PatternMatrix r -> PatternMatrix r
rowSpecialize var con conVars (Matrix rows) = Matrix $ foldl' f Empty rows
  where
    f acc row = case Map.lookup var row.cols of
      -- add the row as is since it's wildcard
      -- because it places no constraints on `var`
      Nothing -> acc :|> row
      -- expand the or pattern into multiple rows
      Just (POr qs p) -> expandOr var qs p row f acc
      -- delete the column since there are no sub patterns
      Just (PData con' Seq.Empty) | con == con' ->
        acc :|> (row & rColsL %~ Map.delete var)
      -- otherwise replace the column with its sub patterns
      Just (PData con' ps) | con == con' -> do
        let g row i p = handlePattern (conVars `Seq.index` i) p row id
        (acc :|>) $ Seq.foldlWithIndex
          g (row & rColsL %~ Map.delete var) ps
      -- since the constrcutors don't match delete the row
      _ -> acc
```

``` haskell
patternTypeInfo :: Pattern 'Low -> TypeInfo
patternTypeInfo (PData (DataCon _ _ t) _) = t
patternTypeInfo (POr (NESeq p _) _ )      = patternTypeInfo p

-- NOTE: partial Map.!, foldl1
columnPick :: PatternMatrix r -> (Var, TypeInfo)
columnPick (Matrix rows@(Row vs _ _ _ :<| _)) =
  let (k, _) = List.foldl1 findMax . Map.toList $
        foldl' mkMap (vs $> (0 :: Int)) rows in
  (k, patternTypeInfo $ vs Map.! k)
  where
    findMax a b
      | snd b > snd a = b
      | otherwise     = a
    mkMap acc (Row m _ _ _) =
      Map.foldlWithKey' (\acc k _ -> Map.insertWith (+) k 1 acc) acc m
columnPick (Matrix Empty) = error "empty match matrix"

matchComplete :: TypeInfo -> Set DataCon -> Bool
matchComplete (TypeInfo (Just span)) cons = length cons == fromIntegral span
matchComplete (TypeInfo Nothing) _        = False
```

``` haskell
matchCompile :: (Map Text Var -> r -> r') -> PatternMatrix r -> MatchM (Match r')
matchCompile _ (Matrix Empty) = pure Fail
matchCompile k (Matrix (Row col Nothing binds body :<| _))
  | null col = pure $ Leaf (k binds body)
matchCompile k (Matrix (Row col (Just (v, p)) binds body :<| rs))
  -- NOTE: we should scan the remaining rows for test involving v, including
  -- guards since there is no need to perform tests later on that are impossible
  | null col = do
    error "TODO: is this actually valid?????"
    let row = Row (Map.fromList [(v, p)]) Nothing binds body in
      matchCompile k (Matrix $ row :<| rs)
matchCompile k m@(Matrix rows) = do
  -- pick a variable to scrutinize
  let (var, tinfo) = columnPick m
  -- collect the heads of constructors tested against var
  -- then compile the subtrees and combine them into a switch
  (def, cons, cases) <- foldM (goRow var) (Nothing, mempty, Empty) rows
  case def of
    Just {} ->
      pure $ Switch var cases def
    Nothing | matchComplete tinfo cons ->
      pure $ Switch var cases Nothing
    Nothing ->
      Switch var cases . Just
        <$> defaultCase var
  where
    defaultCase v = matchCompile k (rowDefault v m)

    specialize v acc@(d, cs, ms) c = do
      if Set.notMember c cs then do
        xs <- matchConVars c
        let body = rowSpecialize v c xs m
        m <- Alt c xs <$> shift (length xs) (matchCompile k body)
        pure (d, Set.insert c cs, ms :|> m)
      else pure acc

    -- for each constructor, specialize the matrix filtering out repeated
    -- test against constructors
    goHead _ acc@(Just {}, _, _) _ = pure acc
    goHead v acc (PData c _)     = specialize v acc c
    goHead v acc (POr qs p)      = do
      acc <- foldM (goHead v) acc qs
      -- check if we hit a wildcard while compiling subpatterns
      case fst $ lower p of
        -- compile a default case
        Nothing -> defaultCase v <&> \d ->
          acc & _1 ?~ d
        -- compile the generalized pattern
        Just q -> goHead v acc q

    goRow _  acc@(Just {}, _, _) _ = pure acc
    goRow v acc r = do
      case Map.lookup v r.cols of
        Just q -> goHead v acc q
        _      -> pure acc
```

# Prettyprinter Instances

``` haskell
vsep :: (Foldable f) => f (P.Doc ann) -> P.Doc ann
vsep = P.concatWith (\x y -> x <> P.line <> y)

sep :: (Foldable f) => f (P.Doc ann) -> P.Doc ann
sep = P.group . vsep

hsepBy :: (Foldable f) => P.Doc ann -> f (P.Doc ann) -> P.Doc ann
hsepBy z = P.concatWith (\x y -> x <> z P.<+> y)

instance P.Pretty Var where
  pretty (Var v) = "$" <> P.pretty v

instance P.Pretty DataCon where
  pretty (DataCon c _ _) = P.pretty c

instance (P.Pretty r) => P.Pretty (Alt r) where
  pretty (Alt c xs rhs) =
    sep (P.pretty c :<| (P.pretty <$> xs))
      <+> "->"
      <+>  P.pretty rhs

instance (P.Pretty r) => P.Pretty (Match r) where
  pretty (Switch x as d) =
    P.nest 2 (vsep $ k (m x as) d) <> P.line <> "}"
    where
      m v ps = "match" <+> P.pretty v <+> "{" :<| fmap P.pretty ps
      k m = maybe m ((m :|>) . ("_ ->" <+>) . P.pretty)
  pretty (Leaf x) = P.pretty x
  pretty Fail     = "FAIL"

-- NOTE: move this somewhere
data Leaf r = Body (Map Text Var) r

instance (P.Pretty r) => P.Pretty (Leaf r) where
  pretty (Body bs x) =
    P.brackets (hsepBy "," (go <$> Map.toList bs))
      <+> P.pretty x
    where
      go (x, v) = P.pretty v <> "/" <> P.pretty x
```

# References {#refs}
