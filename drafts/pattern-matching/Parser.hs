{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

module Parser
    ( parse
    , parser
    ) where

import Control.Applicative
import Control.Applicative.Combinators.NonEmpty (sepBy1)
import Control.Monad
import Control.Monad.Combinators.Expr           (Operator (..), makeExprParser)
import Control.Monad.Reader
import Data.Char                                qualified as Char
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty                       (NonEmpty (..))
import Data.List.NonEmpty                       qualified as NList
import Data.Set                                 qualified as Set
import Data.Text                                qualified as Text
import Data.Text.Short                          qualified as TextS
import Data.Void                                (Void)
import Lang
import Text.Megaparsec                          qualified as Mega
import Text.Megaparsec.Char                     qualified as Mega
import Text.Megaparsec.Char.Lexer               qualified as MegaL

type Parsec = Mega.ParsecT Void StrictText (Reader (Mega.Pos, Mega.Pos))

indentLocal :: Parsec a -> Parsec a
indentLocal m = do
  line <- Mega.sourceLine <$> Mega.getSourcePos
  -- flip local m \rPos@(rLine, rCol) ->
  --   if line == rLine then rPos
  --   else (line, tabWidth <> rCol)
  flip local m \(_, rCol) ->
    (line, tabWidth <> rCol)

-- indentLevel :: Parsec Mega.Pos
-- indentLevel = snd <$> ask

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

ident :: Parsec Char -> Parsec RawName
ident first = fmap TextS.fromText . nblexeme . Mega.try $ (name >>= check)
  where
    firstChar = first <|> Mega.char '_'
    keywords = Set.fromList ["let", "in", "_", "case", "data", "rec", "where", "of", "primitive"]
    name = Mega.lookAhead firstChar *> Mega.takeWhileP Nothing identChar
    check x
      | x `Set.member` keywords = fail $ show x ++ " cannot be an identifier"
      | otherwise = pure x

pChar :: Parsec Char
pChar = Mega.between (Mega.char '\'') (Mega.char '\'') MegaL.charLiteral Mega.<?> "character literal"

pString :: Parsec StrictText
pString = Text.pack <$> (Mega.char '"' *> Mega.manyTill MegaL.charLiteral (Mega.char '"')) Mega.<?> "string literal"

pVar :: Parsec RawName
pVar =  Mega.label "variable" $ Mega.choice
  [ TextS.fromText <$> escape
  , ident Mega.lowerChar
  ]
  where escape = Mega.char '@' *> pString

pCon :: Parsec RawName
pCon = Mega.label "consructor" $ Mega.choice
  [ TextS.fromText <$> escape
  , ident Mega.upperChar
  ]
  where escape = Mega.char '@' *> Mega.char '@' *> pString

pType :: Parsec RawType
pType =
  makeExprParser
    (Mega.choice [parens pType, RTyCon <$> nblexeme pCon])
    [[InfixR (symbol "->" $> RTyFun)]]

pLit :: Parsec Literal
pLit = Mega.label "literal" . nblexeme $ Mega.choice
  [ LChar <$> pChar
  , LString <$> pString
  , pInt
  ]
  where
    pNum = Mega.try (Mega.char '0' >> Mega.choice
      [ Mega.char' 'b' >> MegaL.binary
      , Mega.char' 'o' >> MegaL.octal
      , Mega.char' 'x' >> MegaL.hexadecimal
      ]) <|> MegaL.decimal
    pInt = Mega.try do
      LInt <$> MegaL.signed empty pNum Mega.<?> "integer literal"

pPattern :: Parsec RawPattern
pPattern = makeExprParser pPattern' [[oAs], [oAnnot], [oOr]]
  where
    pPattern' = Mega.choice
      [ parens pPattern
      , RPData <$> pCon <*> many pPattern
      , RPLit <$> pLit
      , rsymbol "_" $> RPWild
      , RPVar <$> pVar
      ]

    oAs = Prefix do
      x <- Mega.try (pVar <* Mega.char '@')
      pure (`RPAs` x)

    oAnnot = Postfix do
      t <- symbol ":" *> pType
      pure (`RPAnnot` t)

    oOr = InfixR (symbol "|" $> RPOr)

pTermDef :: Parsec RawTermDef
pTermDef = nblexeme   do
  x <- nblexeme pVar
  Mega.try (pAnno x Mega.<?> "type annotation") <|> do
    e <- symbol "=" *> indentLocal pRaw
    pure (RTDTerm x e) Mega.<?> "term definition"
  where
    pAnno x = do
      t <- symbol ":" *> pType
      pure $ RTDAnno x t

pRaw :: Parsec Raw
pRaw = do
  x <- pExpr
  Mega.optional (symbol ":" *> pType) <&> \case
    Just t -> RAnnot x t
    Nothing -> x
  where
    pAtom = Mega.choice
      [ parens pRaw
      , RLit <$> pLit
      , pData
      , pPrim
      , pLet
      , pLambda
      , pCase
      , RVar <$> Mega.label "variable" pVar
      ]

    pExpr = do
      xs <- lineFold1 pAtom
      pure case xs of
        x :| []          -> x
        RPrim p [] :| xs -> RPrim p xs
        RData d [] :| xs -> RData d xs
        x :| xs          -> RApp x xs

    pData = Mega.label "data constructor" do
      flip RData [] <$> pCon

    pPrim = Mega.label "primitive function" $ flip RPrim [] <$> do
      void $ Mega.char '@'
      TextS.fromText <$>
        Mega.takeWhile1P Nothing identChar

    pLet = Mega.label "let expression" do
      rsymbol "let"
      isRec <- Mega.option False (rsymbol "rec" $> True)
      xs <- NList.toList <$> lineFold1 pTermDef
      (if isRec then RLetRec else RLet) xs
        <$> (symbol "in" *> pRaw)

    pLambda = Mega.label "lambda expression" do
      void . lexeme $ Mega.char '\\'
      xs <- many pVar <* symbol "->"
      RLambda xs <$> pRaw

    pCase = Mega.label "case expression" $ MegaL.indentBlock space do
      l <- MegaL.indentLevel
      e <- rsymbol "case" *> lexeme pRaw <* rsymbol "of"
      pure $ MegaL.IndentMany (Just (tabWidth <> l)) (pure . RCase e) do
        p <- nblexeme pPattern
        void $ symbol "->"
        e <- indentLocal (lexeme pRaw)
        pure (p, e)

-- indentedItems ref lvl p finished = go
--   where
--     go = do
--       space
--       pos <- MegaL.indentLevel
--       Mega.lookAhead (Mega.eof <|> finished) *> pure [] <|> if
--          | pos <= ref -> pure []
--          | pos == lvl -> (:) <$> p <*> go
--          | otherwise  -> MegaL.incorrectIndent EQ lvl pos

pDef :: Parsec RawDef
pDef = MegaL.nonIndented space (pData <|> pTerm <|> pPrim)
  where
    pData = MegaL.indentBlock space do
      c <- rsymbol "data" *> pCon
      pCons c <|> pure (MegaL.IndentNone (RDData c []))

    pCons c = rsymbol "where" $> MegaL.IndentSome (Just (Mega.mkPos 3)) (pure . RDData c) do
      c <- pCon <* symbol ":"
      (ts, t) <- rawTypeSplit <$> nblexeme pType
      pure (c, toList ts, t)

    pTerm = do
      x <- pVar <* symbol ":"
      t <- lexeme pType
      void . MegaL.nonIndented space . nblexeme $ Mega.chunk (TextS.toText x)
      e <- symbol "=" *> indentLocal (lexeme pRaw)
      pure $ RDTerm x t e

    pPrim = do
      rsymbol "primitive"
      void $ Mega.char '@'
      x <- TextS.fromText <$> nblexeme (Mega.takeWhile1P Nothing identChar)
      void $ symbol ":"
      t <- nblexeme pType
      pure $ RDPrim x t

parser :: Parsec [RawDef]
parser = many (lexeme pDef) <* Mega.eof

parse
  :: FilePath -> StrictText
  -> Either (Mega.ParseErrorBundle StrictText Void) [RawDef]
parse file input = runReader
  (Mega.runParserT parser file input)
  (Mega.pos1, Mega.pos1)
