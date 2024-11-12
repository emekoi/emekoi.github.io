{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module Parser
    ( parser
    ) where

import Control.Applicative
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Control.Monad
import Data.Char                                qualified as Char
import Data.Foldable
import Data.Functor
import Data.List                                qualified as List
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

braces :: Parsec a -> Parsec a
braces = Mega.between (symbol "{") (symbol "}")

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

trySubParse :: (Monad m, Alternative m) => m a -> (a -> m a) -> m a
trySubParse p f = p >>= \x -> f x <|> pure x

pVar :: Parsec StrictText
pVar =  ident Mega.lowerChar

pCon :: Parsec StrictText
pCon = ident Mega.upperChar

pArrow :: Parsec ()
pArrow = void $ symbol "->" <|> symbol "→"

pKind :: Parsec RKind
pKind =
  sepBy1 pAtom pArrow <&> \case
    x :| [] -> x
    -- NOTE: we know xs is nonempty so init and last are fine
    x :| xs -> case last xs of
      RKArrow xs' y -> RKArrow (x : (init xs ++ xs')) y
      y             -> RKArrow (x : init xs) y
  where
    pAtom = Mega.choice
      [ parens pKind
      , rsymbol "Type" $> RKType
      , rsymbol "Row" $> RKRow
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
      , pRecord
      ]

    pType1 = do
      xs <- lineFold1 pAtom
      pure case xs of
        x :| [] -> x
        f :| xs -> case f of
          RTApply f xs' -> RTApply f (xs' ++ xs)
          _             -> RTApply f xs

    pType2 = do
      sepBy1 pType1 pArrow <&> \case
        x :| [] -> x
        -- NOTE: we know xs is nonempty so init and last are fine
        x :| xs -> case last xs of
          RTArrow xs' y -> RTArrow (x : (init xs ++ xs')) y
          y             -> RTArrow (x : init xs) y

    pForall = do
      rsymbol "forall" <|> rsymbol "∀"
      (x, t) <- pArg pKind <* symbol "."
      RTForall x t <$> pType

    pRecord = braces do
      Mega.optional (Mega.try p) >>= \case
        Nothing -> Mega.optional pVar >>= \case
          Just r -> pure $ RTRecordExt [] r
          Nothing -> pure $ RTRecord []
        Just x -> do
          xs <- List.sortOn fst <$> many (symbol "," *> p)
          Mega.optional (symbol "|" *> pVar) >>= \case
            Nothing -> pure $ RTRecord (x : xs)
            Just r -> pure $ RTRecordExt (x : xs) r
      where
        p = (,) <$> pVar <*> (symbol ":" *> pType)

pExpr :: Parsec Expr
pExpr = do
  x <- trySubParse pExpr1 pRestrict
  Mega.optional (symbol ":" *> pType) <&> \case
    Just t -> EAnnot x t
    Nothing -> x
  where
    pAtom = Mega.choice
      [ parens pExpr
      , pRecord
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
      xs <- lineFold1 (trySubParse pAtom pSelect)
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

    pSelect x = do
      foldl' ESelect x
        <$> some (Mega.single '.' *> pVar)

    pRestrict x = do
      foldl' ERestrict x
        <$> some (symbol "-" *> pVar)

    pRecord = braces do
      Mega.optional (Mega.try p) >>= \case
        Nothing -> pure $ ERecord []
        Just x -> do
          xs <- many (symbol "," *> p)
          Mega.optional (symbol "|" *> pExpr) >>= \case
            Nothing -> pure $ ERecord (x : xs)
            Just r -> pure $ ERecordExt (x : xs) r
      where
        p = (,) <$> pVar <*> (symbol "=" *> pExpr)

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
