{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module Parser
    ( parser
    ) where

import Control.Applicative
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Control.Monad
import Control.Monad.Combinators.Expr           (Operator (..), makeExprParser)
import Data.Char                                qualified as Char
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty                       (NonEmpty (..))
import Data.Set                                 qualified as Set
import Data.Void                                (Void)
import Syntax
import Text.Megaparsec                          qualified as Mega
import Text.Megaparsec.Char                     qualified as Mega
import Text.Megaparsec.Char.Lexer               qualified as MegaL

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
pArrow = rsymbol "->" <|> rsymbol "→"

pKind :: Parsec RKind
pKind =
  makeExprParser
    (Mega.choice [parens pKind, rsymbol "Type" $> RKType])
    [[InfixR (pArrow $> RKArrow)]]

pType :: Parsec RType
pType =
  makeExprParser choices
    [[InfixR (pArrow $> RTArrow)]]
  where
    choices = Mega.choice
      [ parens pType
      , RTCon <$> nblexeme pCon
      , RTVar <$> pVar
      , pForall
      ]

    pVar' = (, Nothing) <$> pVar <|> parens do
      (,) <$> pVar <*> fmap Just (rsymbol ":" *> pKind)

    pForall = do
      rsymbol "forall" <|> rsymbol "∀"
      uncurry RTForall
        <$> pVar'
        <*> (rsymbol "." *> pType)

pExpr :: Parsec Expr
pExpr = do
  x <- pExpr1
  Mega.optional (rsymbol ":" *> pType) <&> \case
    Just t -> EAnnot x t
    Nothing -> x
  where
    pAtom = Mega.choice
      [ parens pExpr
      , pLet
      , pLambda
      , EVar <$> pVar
      ]

    pExpr1 = do
      xs <- lineFold1 pAtom
      pure case xs of
        x :| [] -> x
        x :| xs -> foldl' EApply x xs

    pLet = do
      rsymbol "let"
      isRec <- Mega.option False (rsymbol "rec" $> True)
      x <- pVar
      t <- optional (rsymbol ":" *> pType)
      v <- rsymbol "=" *> lexeme pExpr
      e <- rsymbol "in" *> pExpr
      pure $ (if isRec then ELetRec else ELet) x t v e

    pLambda = do
      void . lexeme $ (Mega.char '\\' <|> Mega.char 'λ')
      k <- (flip ELambda Nothing <$> pVar) <|> parens do
        ELambda <$> pVar <*> fmap Just (rsymbol ":" *> pType)
      pArrow
      k <$> pExpr

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
