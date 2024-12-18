{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}

-- |
-- Module      :  Text.MMark.Parser
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- MMark markdown parser.
module Text.MMark.Parser
    ( MMarkErr (..)
    , parse
    ) where

import Control.Applicative                hiding (many, some)
import Control.Monad
import Control.Monad.Combinators.NonEmpty qualified as NE
import Data.Aeson                         qualified as Aeson
import Data.Bifunctor                     (Bifunctor (..))
import Data.Bool                          (bool)
import Data.Char                          qualified as Char
import Data.DList                         qualified as DList
import Data.HashMap.Strict                qualified as HM
import Data.HTML.Entities                 (htmlEntityMap)
import Data.List.NonEmpty                 (NonEmpty (..), (<|))
import Data.List.NonEmpty                 qualified as NE
import Data.Map.Strict                    qualified as Map
import Data.Maybe                         (catMaybes, fromJust, isJust,
                                           isNothing)
import Data.Monoid                        (Any (..), Last (..))
import Data.Ratio                         ((%))
import Data.Set                           qualified as Set
import Data.Text                          (Text)
import Data.Text                          qualified as T
import Data.Text.Encoding                 qualified as TE
import Lens.Micro                         ((^.))
import Text.Email.Validate                qualified as Email
import Text.Megaparsec                    hiding (State (..), parse)
import Text.Megaparsec.Char               hiding (eol)
import Text.Megaparsec.Char.Lexer         qualified as L
import Text.MMark.Parser.Internal
import Text.MMark.Type
import Text.MMark.Util
import Text.URI                           (URI)
import Text.URI                           qualified as URI
import Text.URI.Lens                      (uriPath)

#if !defined(ghcjs_HOST_OS)
import Data.Yaml                          qualified as Yaml
#endif

----------------------------------------------------------------------------
-- Auxiliary data types

-- | Frame that describes where we are in parsing inlines.
data InlineFrame
  -- | Emphasis with asterisk @*@
  = EmphasisFrame
  -- | Emphasis with underscore @_@
  | EmphasisFrame_
  -- | Strong emphasis with asterisk @**@
  | StrongFrame
  -- | Strong emphasis with underscore @__@
  | StrongFrame_
  -- | Strikeout
  | StrikeoutFrame
  -- | Subscript
  | SubscriptFrame
  -- | Superscript
  | SuperscriptFrame
  deriving (Eq, Ord, Show)

-- | State of inline parsing that specifies whether we expect to close one
-- frame or there is a possibility to close one of two alternatives.
data InlineState
  -- | One frame to be closed
  = SingleFrame InlineFrame
  -- | Two frames to be closed
  | DoubleFrame InlineFrame InlineFrame
  deriving (Eq, Ord, Show)

----------------------------------------------------------------------------
-- Top-level API

-- | Parse a markdown document in the form of a strict 'Text' value and
-- either report parse errors or return an 'MMark' document.
parse ::
  Monad m =>
  -- | File name (only to be used in error messages), may be empty
  FilePath ->
  -- | Input to parse
  Text ->
  -- | Parse errors or parsed document
  Either (ParseErrorBundle Text MMarkErr) (MMark m)
parse file input =
  case runBParser pMMark file input of
    Left bundle -> Left bundle
    Right ((myaml, rawBlocks), defs) ->
      let parsed = doInline <$> rawBlocks
          doInline =
            fmap $
              first (replaceEof "end of inline block")
                . runIParser defs pInlinesTop
          e2p = either DList.singleton (const DList.empty)
       in case NE.nonEmpty . DList.toList $ foldMap (foldMap e2p) parsed of
            Nothing ->
              Right
                MMark
                  { mmarkYaml = myaml,
                    mmarkBlocks = fmap fromRight <$> parsed,
                    mmarkExtension = mempty
                  }
            Just errs ->
              Left
                ParseErrorBundle
                  { bundleErrors = errs,
                    bundlePosState =
                      PosState
                        { pstateInput = input,
                          pstateOffset = 0,
                          pstateSourcePos = initialPos file,
                          pstateTabWidth = mkPos 4,
                          pstateLinePrefix = ""
                        }
                  }

----------------------------------------------------------------------------
-- Block parser

-- | Parse an MMark document on block level.
pMMark :: BParser (Maybe Aeson.Object, [Block Isp])
pMMark = do
  meyaml <- optional pYamlBlock
  blocks <- pBlocks
  eof
  return $ case meyaml of
    Just (Left (o, err)) ->
      (Nothing, prependErr o (YamlParseError err) blocks)
    Just (Right (Aeson.Object yaml)) ->
      (Just yaml, blocks)
    _ ->
      (Nothing, blocks)

-- | Parse a set of attributes.
pAttributes :: MonadParsec MMarkErr Text m => m Attributes
pAttributes = between (char '{') (char '}') $ do
  mconcat <$> some ((pClass <|> pId <|> pPair) <* sc)
  where
    isTokenChar c = not $ isAsciiPunctuation c && c /= '-'
      || isSpaceN c
      || c `elem` ['#', '.', '"']

    xmlCompat c = Char.isAsciiLower c && c /= ':'
    isValueChar c = Char.isAscii c &&
      (Char.isAlphaNum c || c `elem` ['_', '-', ':'])
    pString =
      between (char '"') (char '"') $
        let f x = x /= '"' in
          manyEscapedWith f "unescaped character"
    pClass = do
      c <- char '.' >> takeWhile1P (Just "HTML class") isTokenChar
      pure mempty { classes = [c] }
    pId = do
      i <- char '#' >> takeWhile1P (Just "HTML id") isTokenChar
      pure mempty { identifier = Last (Just i) }
    pPair = do
      key <- takeWhile1P (Just "HTML attribute") xmlCompat
      void $ sc *> char '=' <* sc
      value <- pString <|> takeWhile1P (Just "HTML attribute value") isValueChar
      pure mempty { pairs = Map.singleton key value }

-- | Parse a YAML block. On success return the actual parsed 'Aeson.Value' in
-- 'Right', otherwise return 'SourcePos' of parse error and 'String'
-- describing the error as generated by the @yaml@ package in 'Left'.
pYamlBlock :: BParser (Either (Int, String) Aeson.Value)
pYamlBlock = do
  string "---" *> sc' *> eol
  let go acc = do
        l <- takeWhileP Nothing notNewline
        void (optional eol)
        e <- atEnd
        if e || T.stripEnd l == "---"
          then return acc
          else go (acc . (l :))
  doffset <- getOffset
  ls <- go id <*> ([] <$ sc)
  return $ decodeYaml ls doffset

-- | Parse several (possibly zero) blocks in a row.
pBlocks :: BParser [Block Isp]
pBlocks = catMaybes <$> many pBlock

-- | Parse a single block of markdown document.
pBlock :: BParser (Maybe (Block Isp))
pBlock = do
  sc
  rlevel <- refLevel
  alevel <- L.indentLevel
  done <- atEnd
  if done || alevel < rlevel
    then empty
    else case compare alevel (ilevel rlevel) of
      LT ->
        choice
          [ Just <$> pThematicBreak,
            Just <$> pAtxHeading,
            Just <$> pFencedCodeBlock,
            Just <$> pFencedDiv,
            Just <$> pTable,
            Just <$> pUnorderedList,
            Just <$> pOrderedList,
            Just <$> pBlockquote,
            Just <$> pNoteDef,
            pReferenceDef,
            Just <$> pParagraph
          ]
      _ ->
        Just <$> pIndentedCodeBlock

-- | Parse a thematic break.
pThematicBreak :: BParser (Block Isp)
pThematicBreak = do
  l' <- lookAhead nonEmptyLine
  let l = T.filter (not . isSpace) l'
  if T.length l >= 3
    && ( T.all (== '*') l
           || T.all (== '-') l
           || T.all (== '_') l
       )
    then ThematicBreak <$ nonEmptyLine <* sc
    else empty

-- | Parse an ATX heading.
pAtxHeading :: BParser (Block Isp)
pAtxHeading = do
  (void . lookAhead . try) hashIntro
  withRecovery recover $ do
    hlevel <- length <$> hashIntro
    sc1'
    ispOffset <- getOffset
    r <-
      someTill (satisfy notNewline <?> "heading character") . try $
        optional (sc1' *> some (char '#') *> sc') *> (eof <|> eol)
    let toBlock = case hlevel of
          1 -> Heading1
          2 -> Heading2
          3 -> Heading3
          4 -> Heading4
          5 -> Heading5
          _ -> Heading6
    toBlock (IspSpan ispOffset (T.strip (T.pack r))) <$ sc
  where
    hashIntro = count' 1 6 (char '#')
    recover err =
      Heading1 (IspError err) <$ takeWhileP Nothing notNewline <* sc

-- | Parse a fenced code block.
pFencedCodeBlock :: BParser (Block Isp)
pFencedCodeBlock = do
  alevel <- L.indentLevel
  (ch, n, infoString) <- pOpeningFence
  let content = label "code block content" (option "" nonEmptyLine <* eol)
  ls <- manyTill content (pClosingFence ch n)
  CodeBlock infoString (assembleCodeBlock alevel ls) <$ sc

-- | Parse the opening fence of a fenced code block.
pOpeningFence :: BParser (Char, Int, Attributes)
pOpeningFence = p '`' <|> p '~'
  where
    pSimple = do
      ml <- optional
        (T.strip <$> someEscapedWith (not . isSpaceN) <?> "info string")
      guard (maybe True (not . T.any (== '`')) ml)
      pure $ case ml of
        Nothing -> mempty
        Just l ->
          if T.null l
            then mempty
            else mempty { classes = [l] }
    p ch = try $ do
      void $ count 3 (char ch)
      n <- (+ 3) . length <$> many (char ch)
      attrs <- sc' *> (try pAttributes <|> do
        (pSimple <* sc') <> (try pAttributes <|> mempty))
      (ch, n, attrs) <$ eol

-- | Parse the closing fence of a fenced block.
pClosingFence :: Char -> Int -> BParser ()
pClosingFence ch n = try . label "closing code fence" $ do
  clevel <- ilevel <$> refLevel
  void $ L.indentGuard sc' LT clevel
  void $ count n (char ch)
  (void . many . char) ch
  sc'
  eof <|> eol

-- | Parse an indented code block.
pIndentedCodeBlock :: BParser (Block Isp)
pIndentedCodeBlock = do
  alevel <- L.indentLevel
  clevel <- ilevel <$> refLevel
  let go ls = do
        indented <-
          lookAhead $
            (>= clevel) <$> (sc *> L.indentLevel)
        if indented
          then do
            l <- option "" nonEmptyLine
            continue <- eol'
            let ls' = ls . (l :)
            if continue
              then go ls'
              else return ls'
          else return ls
      -- NOTE This is a bit unfortunate, but it's difficult to guarantee
      -- that preceding space is not yet consumed when we get to
      -- interpreting input as an indented code block, so we need to restore
      -- the space this way.
      f x = T.replicate (unPos alevel - 1) " " <> x
      g []       = []
      g (x : xs) = f x : xs
  ls <- g . ($ []) <$> go id
  CodeBlock mempty (assembleCodeBlock clevel ls) <$ sc

-- | Parse an unorederd list.
pUnorderedList :: BParser (Block Isp)
pUnorderedList = do
  (bullet, bulletPos, minLevel, indLevel) <-
    pListBullet Nothing
  x <- innerBlocks bulletPos minLevel indLevel
  xs <- many $ do
    (_, bulletPos', minLevel', indLevel') <-
      pListBullet (Just (bullet, bulletPos))
    innerBlocks bulletPos' minLevel' indLevel'
  return (UnorderedList (normalizeListItems (x :| xs)))
  where
    innerBlocks bulletPos minLevel indLevel = do
      p <- getSourcePos
      let tooFar = sourceLine p > sourceLine bulletPos <> pos1
          rlevel = slevel minLevel indLevel
      if tooFar || sourceColumn p < minLevel
        then return [bool Naked Paragraph tooFar emptyIspSpan]
        else subEnv True rlevel pBlocks

-- | Parse a list bullet. Return a tuple with the following components (in
-- order):
--
--     * 'Char' used to represent the bullet
--     * 'SourcePos' at which the bullet was located
--     * the closest column position where content could start
--     * the indentation level after the bullet
pListBullet ::
  -- | Bullet 'Char' and start position of the first bullet in a list
  Maybe (Char, SourcePos) ->
  BParser (Char, SourcePos, Pos, Pos)
pListBullet mbullet = try $ do
  pos <- getSourcePos
  l <- (<> mkPos 2) <$> L.indentLevel
  bullet <-
    case mbullet of
      Nothing -> char '-' <|> char '+' <|> char '*'
      Just (bullet, bulletPos) -> do
        guard (sourceColumn pos >= sourceColumn bulletPos)
        char bullet
  eof <|> sc1
  l' <- L.indentLevel
  return (bullet, pos, l, l')

-- | Parse an ordered list.
pOrderedList :: BParser (Block Isp)
pOrderedList = do
  startOffset <- getOffset
  (startIx, del, startPos, minLevel, indLevel) <-
    pListIndex Nothing
  x <- innerBlocks startPos minLevel indLevel
  xs <- manyIndexed (startIx + 1) $ \expectedIx -> do
    startOffset' <- getOffset
    (actualIx, _, startPos', minLevel', indLevel') <-
      pListIndex (Just (del, startPos))
    let f blocks =
          if actualIx == expectedIx
            then blocks
            else
              prependErr
                startOffset'
                (ListIndexOutOfOrder actualIx expectedIx)
                blocks
    f <$> innerBlocks startPos' minLevel' indLevel'
  return . OrderedList startIx . normalizeListItems $
    ( if startIx <= 999999999
        then x
        else prependErr startOffset (ListStartIndexTooBig startIx) x
    )
      :| xs
  where
    innerBlocks indexPos minLevel indLevel = do
      p <- getSourcePos
      let tooFar = sourceLine p > sourceLine indexPos <> pos1
          rlevel = slevel minLevel indLevel
      if tooFar || sourceColumn p < minLevel
        then return [bool Naked Paragraph tooFar emptyIspSpan]
        else subEnv True rlevel pBlocks

-- | Parse a list index. Return a tuple with the following components (in
-- order):
--
--     * 'Word' parsed numeric index
--     * 'Char' used as delimiter after the numeric index
--     * 'SourcePos' at which the index was located
--     * the closest column position where content could start
--     * the indentation level after the index
pListIndex ::
  -- | Delimiter 'Char' and start position of the first index in a list
  Maybe (Char, SourcePos) ->
  BParser (Word, Char, SourcePos, Pos, Pos)
pListIndex mstart = try $ do
  pos <- getSourcePos
  i <- L.decimal
  del <- case mstart of
    Nothing -> char '.' <|> char ')'
    Just (del, startPos) -> do
      guard (sourceColumn pos >= sourceColumn startPos)
      char del
  l <- (<> pos1) <$> L.indentLevel
  eof <|> sc1
  l' <- L.indentLevel
  return (i, del, pos, l, l')

-- | Parse a block quote.
pBlockquote :: BParser (Block Isp)
pBlockquote = do
  minLevel <- try $ do
    minLevel <- (<> pos1) <$> L.indentLevel
    void (char '>')
    eof <|> sc
    l <- L.indentLevel
    return $
      if l > minLevel
        then minLevel <> pos1
        else minLevel
  indLevel <- L.indentLevel
  if indLevel >= minLevel
    then do
      let rlevel = slevel minLevel indLevel
      xs <- subEnv False rlevel pBlocks
      return (Blockquote xs)
    else return (Blockquote [])

-- | Parse a link\/image reference definition and register it.
pReferenceDef :: BParser (Maybe (Block Isp))
pReferenceDef = do
  (o, dlabel) <- try (pRefLabel <* char ':')
  withRecovery recover $ do
    sc' <* optional eol <* sc'
    uri <- pUri
    hadSpN <-
      optional $
        (sc1' *> option False (True <$ eol)) <|> (True <$ (sc' <* eol))
    sc'
    mtitle <-
      if isJust hadSpN
        then optional pTitle <* sc'
        else return Nothing
    case (hadSpN, mtitle) of
      (Just True, Nothing) -> return ()
      _                    -> hidden eof <|> eol
    conflict <- registerReference dlabel (uri, mtitle)
    when conflict $
      customFailure' o (DuplicateReferenceDefinition dlabel)
    Nothing <$ sc
  where
    recover err =
      Just (Naked (IspError err)) <$ takeWhileP Nothing notNewline <* sc

-- | Parse a pipe table.
pTable :: BParser (Block Isp)
pTable = do
  (n, headerRow) <- try $ do
    pos <- L.indentLevel
    option False (T.any (== '|') <$> lookAhead nonEmptyLine) >>= guard
    let pipe' = option False (True <$ pipe)
    l <- pipe'
    headerRow <- NE.sepBy1 cell (try (pipe <* notFollowedBy eol))
    r <- pipe'
    let n = NE.length headerRow
    guard (n > 1 || l || r)
    eol <* sc'
    L.indentLevel >>= \i -> guard (i == pos || i == (pos <> pos1))
    lookAhead nonEmptyLine >>= guard . isHeaderLike
    return (n, headerRow)
  withRecovery recover $ do
    sc'
    caligns <- rowWrapper (NE.fromList <$> sepByCount n calign pipe)
    otherRows <- many $ do
      endOfTable >>= guard . not
      rowWrapper (NE.fromList <$> sepByCount n cell pipe)
    Table caligns (headerRow :| otherRows) <$ sc
  where
    cell = do
      o <- getOffset
      txt <-
        fmap (T.stripEnd . T.pack) . foldMany' . choice $
          [ (++) . T.unpack <$> hidden (string "\\|"),
            (++) . T.unpack <$> pCodeSpanB,
            (:) <$> label "inline content" (satisfy cellChar)
          ]
      return (IspSpan o txt)
    cellChar x = x /= '|' && notNewline x
    rowWrapper p = do
      void (optional pipe)
      r <- p
      void (optional pipe)
      eof <|> eol
      sc'
      return r
    pipe = char '|' <* sc'
    calign = do
      let colon' = option False (True <$ char ':')
      l <- colon'
      void (count 3 (char '-') <* many (char '-'))
      r <- colon'
      sc'
      return $
        case (l, r) of
          (False, False) -> CellAlignDefault
          (True, False)  -> CellAlignLeft
          (False, True)  -> CellAlignRight
          (True, True)   -> CellAlignCenter
    isHeaderLike txt =
      T.length (T.filter isHeaderConstituent txt) % T.length txt
        > 8 % 10
    isHeaderConstituent x =
      isSpace x || x == '|' || x == '-' || x == ':'
    endOfTable =
      lookAhead (option True (isBlank <$> nonEmptyLine))
    recover err =
      Naked (IspError (replaceEof "end of table block" err))
        <$ manyTill
          (optional nonEmptyLine)
          (endOfTable >>= guard)
        <* sc

-- | Parse a paragraph or naked text (in some cases).
pParagraph :: BParser (Block Isp)
pParagraph = do
  startOffset <- getOffset
  allowNaked <- isNakedAllowed
  rlevel <- refLevel
  let go ls = do
        l <- lookAhead (option "" nonEmptyLine)
        broken <- succeeds . lookAhead . try $ do
          sc
          alevel <- L.indentLevel
          guard (alevel < ilevel rlevel)
          unless (alevel < rlevel) . choice $
            [ void (char '>'),
              void (char ':'),
              void pThematicBreak,
              void pAtxHeading,
              void pOpeningFence,
              void (pListBullet Nothing),
              void (pListIndex Nothing)
            ]
        if isBlank l
          then return (ls, Paragraph)
          else
            if broken
              then return (ls, Naked)
              else do
                void nonEmptyLine
                continue <- eol'
                let ls' = ls . (l :)
                if continue
                  then go ls'
                  else return (ls', Naked)
  l <- nonEmptyLine
  continue <- eol'
  (ls, toBlock) <-
    if continue
      then go id
      else return (id, Naked)
  (if allowNaked then toBlock else Paragraph)
    (IspSpan startOffset (assembleParagraph (l : ls [])))
    <$ sc

-- | Parse a fenced div
pFencedDiv :: BParser (Block Isp)
pFencedDiv = do
  skipCount 3 (char ':')
  n <- length <$> many (char ':')
  attrs <- (sc' *> (pAttributes <|> pure mempty) <* eol) <?> "div attributes"
  sc
  xs <- catMaybes <$> manyTill pBlock (pClosingDivFence n)
  Div attrs xs <$ sc

-- | Parse the closing fence of a fenced div.
pClosingDivFence :: Int -> BParser ()
pClosingDivFence n = try . label "closing div fence" $ do
  clevel <- ilevel <$> refLevel
  void $ L.indentGuard sc' LT clevel
  skipCount (n + 3) (char ':')
  skipMany (char ':')
  sc' *> (eof <|> eol)

-- | Parse a footnote definition.
pNoteDef :: BParser (Block Isp)
pNoteDef = do
  (o, dlabel) <- try (pNoteLabel <* char ':')
  conflict <- registerFootnote dlabel
  when conflict $
    customFailure' o (DuplicateFootnoteDefinition dlabel)

  minLevel <- try $ do
    minLevel <- (<> pos1) <$> L.indentLevel
    eof <|> sc
    l <- L.indentLevel
    return $
      if l > minLevel
        then minLevel <> pos1
        else minLevel
  indLevel <- L.indentLevel

  Note dlabel <$> if indLevel >= minLevel
    then do
      let rlevel = slevel minLevel indLevel
      subEnv False rlevel pBlocks
    else pure []

----------------------------------------------------------------------------
-- Auxiliary block-level parsers

-- | 'match' a code span, this is a specialised and adjusted version of
-- 'pCodeSpan'.
pCodeSpanB :: BParser Text
pCodeSpanB = fmap fst . match . hidden $ do
  n <- try (length <$> some (char '`'))
  let finalizer = try $ do
        void $ count n (char '`')
        notFollowedBy (char '`')
  skipManyTill
    ( label "code span content" $
        takeWhile1P Nothing (== '`')
          <|> takeWhile1P Nothing (\x -> x /= '`' && notNewline x)
    )
    finalizer

----------------------------------------------------------------------------
-- Inline parser

-- | The top level inline parser.
pInlinesTop :: IParser (NonEmpty Inline)
pInlinesTop = do
  inlines <- pInlines
  eof <|> void pLfdr
  return inlines

-- | Parse inlines using settings from given 'InlineConfig'.
pInlines :: IParser (NonEmpty Inline)
pInlines = do
  done <- atEnd
  allowsEmpty <- isEmptyAllowed
  if done
    then
      if allowsEmpty
        then (return . nes . Plain) ""
        else unexpEic EndOfInput
    else NE.some $ do
      mch <- lookAhead (anySingle <?> "inline content")
      case mch of
        '`' -> try pRawInline <|> pCodeSpan
        '[' -> do
          allowsLinks <- isLinksAllowed
          observing (try pSpan) >>= \case
            Left err -> try pNoteRef <|> if allowsLinks
              then pLink <|> parseError err
              -- else unexpEic (Tokens $ nes '[')
              else parseError err
            Right x -> pure x

        '!' -> do
          gotImage <- (succeeds . void . lookAhead . string) "!["
          allowsImages <- isImagesAllowed
          if gotImage
            then
              if allowsImages
                then pImage
                else unexpEic (Tokens . NE.fromList $ "![")
            else pPlain
        '<' -> do
          allowsLinks <- isLinksAllowed
          if allowsLinks
            then try pAutolink <|> pPlain
            else pPlain
        '\\' ->
          try pHardLineBreak <|> pPlain
        '$' -> pMath
        ch ->
          if isFrameConstituent ch
            then pEnclosedInline
            else pPlain

-- | Parse a code span.
--
-- See also: 'pCodeSpanB'.
pCodeSpan :: IParser Inline
pCodeSpan = do
  n <- try (length <$> some (char '`'))
  let finalizer = try $ do
        void $ count n (char '`')
        notFollowedBy (char '`')
  r <-
    CodeSpan . collapseWhiteSpace . T.concat
      <$> manyTill
        ( label "code span content" $
            takeWhile1P Nothing (== '`')
              <|> takeWhile1P Nothing (/= '`')
        )
        finalizer
  r <$ lastChar OtherChar

-- | Parse a link.
pLink :: IParser Inline
pLink = do
  void (char '[')
  o <- getOffset
  txt <- disallowLinks $ disallowEmpty pInlines
  void (char ']')
  (dest, mtitle) <- pLocation o txt
  Link txt dest mtitle <$ lastChar OtherChar

-- | Parse an image.
pImage :: IParser Inline
pImage = do
  (pos, alt) <- emptyAlt <|> nonEmptyAlt
  (src, mtitle) <- pLocation pos alt
  Image alt src mtitle <$ lastChar OtherChar
  where
    emptyAlt = do
      o <- getOffset
      void (string "![]")
      return (o + 2, nes (Plain ""))
    nonEmptyAlt = do
      void (string "![")
      o <- getOffset
      alt <- disallowImages (disallowEmpty pInlines)
      void (char ']')
      return (o, alt)

-- | Parse an autolink.
pAutolink :: IParser Inline
pAutolink = between (char '<') (char '>') $ do
  notFollowedBy (char '>')
  uri' <- URI.parser
  let (txt, uri) =
        case isEmailUri uri' of
          Nothing ->
            ( (nes . Plain . URI.render) uri',
              uri'
            )
          Just email ->
            ( nes (Plain email),
              URI.makeAbsolute mailtoScheme uri'
            )
  Link txt uri Nothing <$ lastChar OtherChar

-- | Parse inline content inside an enclosing construction such as emphasis,
-- strikeout, superscript, and\/or subscript markup.
pEnclosedInline :: IParser Inline
pEnclosedInline =
  disallowEmpty $
    pLfdr >>= \case
      SingleFrame x ->
        liftFrame x <$> pInlines <* pRfdr x
      DoubleFrame x y -> do
        inlines0 <- pInlines
        thisFrame <- pRfdr x <|> pRfdr y
        let thatFrame = if thisFrame == x then y else x
        minlines1 <- optional pInlines
        void (pRfdr thatFrame)
        return . liftFrame thatFrame $
          case minlines1 of
            Nothing ->
              nes (liftFrame thisFrame inlines0)
            Just inlines1 ->
              liftFrame thisFrame inlines0 <| inlines1

-- | Parse a hard line break.
pHardLineBreak :: IParser Inline
pHardLineBreak = do
  void (char '\\')
  eol
  notFollowedBy eof
  sc'
  lastChar SpaceChar
  return LineBreak

-- | Parse plain text.
pPlain :: IParser Inline
pPlain = fmap (Plain . bakeText) . foldSome $ do
  ch <- lookAhead (anySingle <?> "inline content")
  case ch of
    '\\' ->
      (:)
        <$> ( (escapedChar <* lastChar OtherChar)
                <|> try (char '\\' <* notFollowedBy eol <* lastChar OtherChar)
            )
    '\n' ->
      newline'
    '\r' ->
      newline'
    '!' -> do
      notFollowedBy (string "![")
      (:) <$> char '!' <* lastChar PunctChar
    '<' -> do
      notFollowedBy pAutolink
      (:) <$> char '<' <* lastChar PunctChar
    '&' ->
      choice
        [ (:) <$> numRef,
          (++) . reverse <$> entityRef,
          (:) <$> char '&'
        ]
        <* lastChar PunctChar
    _ ->
      (:)
        <$> if Char.isSpace ch
          then char ch <* lastChar SpaceChar
          else
            if isSpecialChar ch
              then
                failure
                  (Just . Tokens . nes $ ch)
                  (Set.singleton . Label . NE.fromList $ "inline content")
              else
                if Char.isPunctuation ch
                  then char ch <* lastChar PunctChar
                  else char ch <* lastChar OtherChar
  where
    newline' =
        (('\n' :) . dropWhile isSpace) <$ eol <* sc' <* lastChar SpaceChar

-- | Parse a math block.
--
-- See also: 'pCodeSpanB'.
pMath :: IParser Inline
pMath = do
  n <- try (length <$> some (char '$'))
  let finalizer = try $ do
        void $ count n (char '$')
        notFollowedBy (char '$')
  guard (n <= 2)
  let style = if n == 1 then InlineMath else DisplayMath
  r <-
    Math style . collapseWhiteSpace . T.concat
      <$> manyTill
        ( label "LaTeX" $
            takeWhile1P Nothing (== '$')
              <|> takeWhile1P Nothing (/= '$')
        )
        finalizer
  r <$ lastChar OtherChar

-- | Parse a span.
pSpan :: IParser Inline
pSpan = do
  void (char '[')
  txt <- disallowEmpty pInlines
  void (char ']')
  attr <- try pAttributes
  Span attr txt <$ lastChar OtherChar

-- | Parse a raw inline.
pRawInline :: IParser Inline
pRawInline = do
  n <- try (length <$> some (char '`'))
  let finalizer = try $ do
        void $ count n (char '`')
        void $ chunk "{=raw}"
  r <-
    RawInline . collapseWhiteSpace . T.concat
      <$> manyTill
        ( label "raw inline content" $
            takeWhile1P Nothing (== '`')
              <|> takeWhile1P Nothing (/= '`')
        )
        finalizer
  r <$ lastChar OtherChar

pNoteRef :: IParser Inline
pNoteRef = do
  (o, dlabel) <- try pNoteLabel
  lookupFootnote dlabel >>= \case
    Left names ->
      customFailure' o (CouldNotFindFootnoteDefinition dlabel names)
    Right _ ->
      pure (NoteRef dlabel)

----------------------------------------------------------------------------
-- Auxiliary inline-level parsers

-- | Parse an inline and reference-style link\/image location.
pLocation ::
  -- | Offset where the content inlines start
  Int ->
  -- | The inner content inlines
  NonEmpty Inline ->
  -- | URI and optionally title
  IParser (URI, Maybe Text)
pLocation innerOffset inner = do
  mr <- optional (inplace <|> withRef)
  case mr of
    Nothing ->
      collapsed innerOffset inner <|> shortcut innerOffset inner
    Just (dest, mtitle) ->
      return (dest, mtitle)
  where
    inplace = do
      void (char '(')
      sc'
      dest <- pUri
      hadSpace <- option False (True <$ sc1)
      mtitle <-
        if hadSpace
          then optional pTitle <* sc'
          else return Nothing
      void (char ')')
      return (dest, mtitle)
    withRef =
      pRefLabel >>= uncurry lookupRef
    collapsed o inlines = do
      region (setErrorOffset o) $
        (void . hidden . string) "[]"
      lookupRef o (mkLabel inlines)
    shortcut o inlines =
      lookupRef o (mkLabel inlines)
    lookupRef o dlabel =
      lookupReference dlabel >>= \case
        Left names ->
          customFailure' o (CouldNotFindReferenceDefinition dlabel names)
        Right x ->
          return x
    mkLabel = T.unwords . T.words . asPlainText

-- | Parse a URI.
pUri :: (MonadParsec e Text m) => m URI
pUri = between (char '<') (char '>') URI.parser <|> naked
  where
    naked = do
      let f x = not (isSpaceN x || x == ')')
          l = "end of URI"
      (s, s') <- T.span f <$> getInput
      when (T.null s) . void $
        (satisfy f <?> "URI") -- this will now fail
      setInput s
      r <- region (replaceEof l) (URI.parser <* label l eof)
      setInput s'
      return r

-- | Parse a title of a link or an image.
pTitle :: (MonadParsec MMarkErr Text m) => m Text
pTitle =
  choice
    [ p '\"' '\"',
      p '\'' '\'',
      p '(' ')'
    ]
  where
    p start end =
      between (char start) (char end) $
        let f x = x /= end
         in manyEscapedWith f "unescaped character"

-- | Parse label of a reference link.
pRefLabel :: (MonadParsec MMarkErr Text m) => m (Int, Text)
pRefLabel = do
  try $ do
    void (char '[')
    notFollowedBy (char ']')
  o <- getOffset
  sc
  let f x = x /= '[' && x /= ']'
  dlabel <- someEscapedWith f <?> "reference label"
  void (char ']')
  return (o, dlabel)

-- | Parse an opening markup sequence corresponding to given 'InlineState'.
pLfdr :: IParser InlineState
pLfdr = try $ do
  o <- getOffset
  let r st = st <$ string (inlineStateDel st)
  st <-
    hidden $
      choice
        [ r (DoubleFrame StrongFrame StrongFrame),
          r (DoubleFrame StrongFrame EmphasisFrame),
          r (SingleFrame StrongFrame),
          r (SingleFrame EmphasisFrame),
          r (DoubleFrame StrongFrame_ StrongFrame_),
          r (DoubleFrame StrongFrame_ EmphasisFrame_),
          r (SingleFrame StrongFrame_),
          r (SingleFrame EmphasisFrame_),
          r (DoubleFrame StrikeoutFrame StrikeoutFrame),
          r (DoubleFrame StrikeoutFrame SubscriptFrame),
          r (SingleFrame StrikeoutFrame),
          r (SingleFrame SubscriptFrame),
          r (SingleFrame SuperscriptFrame)
        ]
  let dels = inlineStateDel st
      failNow =
        customFailure' o (NonFlankingDelimiterRun (toNesTokens dels))
  lch <- getLastChar
  rch <- getNextChar OtherChar
  when (lch >= rch) failNow
  return st

-- | Parse a closing markup sequence corresponding to given 'InlineFrame'.
pRfdr :: InlineFrame -> IParser InlineFrame
pRfdr frame = try $ do
  let dels = inlineFrameDel frame
      expectingInlineContent = region $ \case
        TrivialError pos us es ->
          TrivialError pos us $
            Set.insert (Label $ NE.fromList "inline content") es
        other -> other
  o <- getOffset
  (void . expectingInlineContent . string) dels
  let failNow =
        customFailure' o (NonFlankingDelimiterRun (toNesTokens dels))
  lch <- getLastChar
  rch <- getNextChar SpaceChar
  when (lch <= rch) failNow
  return frame

-- | Get 'CharType' of the next char in the input stream.
getNextChar ::
  -- | What we should consider frame constituent characters
  CharType ->
  IParser CharType
getNextChar frameType = lookAhead (option SpaceChar (charType <$> anySingle))
  where
    charType ch
      | isFrameConstituent ch = frameType
      | Char.isSpace ch = SpaceChar
      | ch == '\\' = OtherChar
      | Char.isPunctuation ch = PunctChar
      | otherwise = OtherChar

-- | Parse label of a footnote.
pNoteLabel :: (MonadParsec MMarkErr Text m) => m (Int, Text)
pNoteLabel = do
  try $ do
    void (char '[')
    void (char '^')
    notFollowedBy (char ']')
  o <- getOffset
  sc
  let f x = x /= '[' && x /= ']'
  dlabel <- someEscapedWith f <?> "reference label"
  void (char ']')
  return (o, dlabel)

----------------------------------------------------------------------------
-- Parsing helpers

manyIndexed :: (Alternative m, Num n) => n -> (n -> m a) -> m [a]
manyIndexed n' m = go n'
  where
    go !n = liftA2 (:) (m n) (go (n + 1)) <|> pure []

foldMany :: (MonadPlus m) => m (a -> a) -> m (a -> a)
foldMany f = go id
  where
    go g =
      optional f >>= \case
        Nothing -> pure g
        Just h -> go (h . g)

foldMany' :: (MonadPlus m) => m ([a] -> [a]) -> m [a]
foldMany' f = ($ []) <$> go id
  where
    go g =
      optional f >>= \case
        Nothing -> pure g
        Just h -> go (g . h)

foldSome :: (MonadPlus m) => m (a -> a) -> m (a -> a)
foldSome f = liftA2 (flip (.)) f (foldMany f)

foldSome' :: (MonadPlus m) => m ([a] -> [a]) -> m [a]
foldSome' f = liftA2 ($) f (foldMany' f)

sepByCount :: (MonadPlus m) => Int -> m a -> m sep -> m [a]
sepByCount 0 _ _   = pure []
sepByCount n p sep = liftA2 (:) p (count (n - 1) (sep *> p))

nonEmptyLine :: BParser Text
nonEmptyLine = takeWhile1P Nothing notNewline

manyEscapedWith ::
  (MonadParsec MMarkErr Text m) =>
  (Char -> Bool) ->
  String ->
  m Text
manyEscapedWith f l =
  fmap T.pack . foldMany' . choice $
    [ (:) <$> escapedChar,
      (:) <$> numRef,
      (++) . reverse <$> entityRef,
      (:) <$> satisfy f <?> l
    ]

someEscapedWith ::
  (MonadParsec MMarkErr Text m) =>
  (Char -> Bool) ->
  m Text
someEscapedWith f =
  fmap T.pack . foldSome' . choice $
    [ (:) <$> escapedChar,
      (:) <$> numRef,
      (++) . reverse <$> entityRef,
      (:) <$> satisfy f
    ]

escapedChar :: (MonadParsec e Text m) => m Char
escapedChar =
  label "escaped character" $
    try (char '\\' *> satisfy isAsciiPunctuation)

-- | Parse an HTML5 entity reference.
entityRef :: (MonadParsec MMarkErr Text m) => m String
entityRef = do
  o <- getOffset
  let f (TrivialError _ us es) = TrivialError o us es
      f (FancyError _ xs)      = FancyError o xs
  name <-
    try . region f $
      between
        (char '&')
        (char ';')
        (takeWhile1P Nothing Char.isAlphaNum <?> "HTML5 entity name")
  case HM.lookup name htmlEntityMap of
    Nothing ->
      customFailure' o (UnknownHtmlEntityName name)
    Just txt -> return (T.unpack txt)

-- | Parse a numeric character using the given numeric parser.
numRef :: (MonadParsec MMarkErr Text m) => m Char
numRef = do
  o <- getOffset
  let f = between (string "&#") (char ';')
  n <- try (f (char' 'x' *> L.hexadecimal)) <|> f L.decimal
  if n == 0 || n > fromEnum (maxBound :: Char)
    then customFailure' o (InvalidNumericCharacter n)
    else return (Char.chr n)

sc :: (MonadParsec e Text m) => m ()
sc = void $ takeWhileP (Just "white space") isSpaceN

sc1 :: (MonadParsec e Text m) => m ()
sc1 = void $ takeWhile1P (Just "white space") isSpaceN

sc' :: (MonadParsec e Text m) => m ()
sc' = void $ takeWhileP (Just "white space") isSpace

sc1' :: (MonadParsec e Text m) => m ()
sc1' = void $ takeWhile1P (Just "white space") isSpace

eol :: (MonadParsec e Text m) => m ()
eol =
  void . label "newline" $
    choice
      [ string "\n",
        string "\r\n",
        string "\r"
      ]

eol' :: (MonadParsec e Text m) => m Bool
eol' = option False (True <$ eol)

----------------------------------------------------------------------------
-- Char classification

isSpace :: Char -> Bool
isSpace x = x == ' ' || x == '\t'

isSpaceN :: Char -> Bool
isSpaceN x = isSpace x || isNewline x

isNewline :: Char -> Bool
isNewline x = x == '\n' || x == '\r'

notNewline :: Char -> Bool
notNewline = not . isNewline

isFrameConstituent :: Char -> Bool
isFrameConstituent = \case
  '*' -> True
  '^' -> True
  '_' -> True
  '~' -> True
  _ -> False

isMarkupChar :: Char -> Bool
isMarkupChar x = isFrameConstituent x || f x
  where
    f = \case
      '[' -> True
      ']' -> True
      '`' -> True
      '$' -> True
      -- ':' -> True
      _ -> False

isSpecialChar :: Char -> Bool
isSpecialChar x = isMarkupChar x || x == '\\' || x == '!' || x == '<'

isAsciiPunctuation :: Char -> Bool
isAsciiPunctuation x =
  (x >= '!' && x <= '/')
    || (x >= ':' && x <= '@')
    || (x >= '[' && x <= '`')
    || (x >= '{' && x <= '~')

----------------------------------------------------------------------------
-- Other helpers

slevel :: Pos -> Pos -> Pos
slevel a l = if l >= ilevel a then a else l

ilevel :: Pos -> Pos
ilevel = (<> mkPos 4)

isBlank :: Text -> Bool
isBlank = T.all isSpace

assembleCodeBlock :: Pos -> [Text] -> Text
assembleCodeBlock indent ls = T.unlines (stripIndent indent <$> ls)

stripIndent :: Pos -> Text -> Text
stripIndent indent txt = T.drop m txt
  where
    m = snd $ T.foldl' f (0, 0) (T.takeWhile isSpace txt)
    f (!j, !n) ch
      | j >= i = (j, n)
      | ch == ' ' = (j + 1, n + 1)
      | ch == '\t' = (j + 4, n + 1)
      | otherwise = (j, n)
    i = unPos indent - 1

assembleParagraph :: [Text] -> Text
assembleParagraph = go
  where
    go []       = ""
    go [x]      = T.dropWhileEnd isSpace x
    go (x : xs) = x <> "\n" <> go xs

collapseWhiteSpace :: Text -> Text
collapseWhiteSpace =
  T.stripEnd . T.filter (/= '\0') . snd . T.mapAccumL f True
  where
    f seenSpace ch =
      case (seenSpace, g ch) of
        (False, False) -> (False, ch)
        (True, False)  -> (False, ch)
        (False, True)  -> (True, ' ')
        (True, True)   -> (True, '\0')
    g ' '  = True
    g '\t' = True
    g '\n' = True
    g _    = False

inlineStateDel :: InlineState -> Text
inlineStateDel = \case
  SingleFrame x -> inlineFrameDel x
  DoubleFrame x y -> inlineFrameDel x <> inlineFrameDel y

liftFrame :: InlineFrame -> NonEmpty Inline -> Inline
liftFrame = \case
  StrongFrame -> Strong
  EmphasisFrame -> Emphasis
  StrongFrame_ -> Strong
  EmphasisFrame_ -> Emphasis
  StrikeoutFrame -> Strikeout
  SubscriptFrame -> Subscript
  SuperscriptFrame -> Superscript

inlineFrameDel :: InlineFrame -> Text
inlineFrameDel = \case
  EmphasisFrame -> "*"
  EmphasisFrame_ -> "_"
  StrongFrame -> "**"
  StrongFrame_ -> "__"
  StrikeoutFrame -> "~~"
  SubscriptFrame -> "~"
  SuperscriptFrame -> "^"

replaceEof :: String -> ParseError Text e -> ParseError Text e
replaceEof altLabel = \case
  TrivialError pos us es -> TrivialError pos (f <$> us) (Set.map f es)
  FancyError pos xs -> FancyError pos xs
  where
    f EndOfInput = Label (NE.fromList altLabel)
    f x          = x

isEmailUri :: URI -> Maybe Text
isEmailUri uri =
  case URI.unRText <$> uri ^. uriPath of
    [x] ->
      if Email.isValid (TE.encodeUtf8 x)
        && ( isNothing (URI.uriScheme uri)
               || URI.uriScheme uri == Just mailtoScheme
           )
        then Just x
        else Nothing
    _ -> Nothing

-- | Decode the yaml block to a 'Aeson.Value'. On GHCJs, without access to
-- libyaml we just return an empty object. It's worth using a pure haskell
-- parser later if this is unacceptable for someone's needs.
decodeYaml :: [T.Text] -> Int -> Either (Int, String) Aeson.Value
#ifdef ghcjs_HOST_OS
decodeYaml _ _ = pure $ Aeson.object []
#else
decodeYaml ls doffset =
  case (Yaml.decodeEither' . TE.encodeUtf8 . T.intercalate "\n") ls of
    Left err' ->
      let (moffset, err) = splitYamlError err'
       in Left (maybe doffset (+ doffset) moffset, err)
    Right v -> Right v

splitYamlError ::
  Yaml.ParseException ->
  (Maybe Int, String)
splitYamlError = \case
  Yaml.NonScalarKey -> (Nothing, "non scalar key")
  Yaml.UnknownAlias anchor -> (Nothing, "unknown alias \"" ++ anchor ++ "\"")
  Yaml.UnexpectedEvent exptd unexptd ->
    ( Nothing,
      "unexpected event: expected " ++ show exptd
        ++ ", but received "
        ++ show unexptd
    )
  Yaml.InvalidYaml myerror -> case myerror of
    Nothing -> (Nothing, "unspecified error")
    Just yerror -> case yerror of
      Yaml.YamlException s -> (Nothing, s)
      Yaml.YamlParseException problem context mark ->
        ( Just (Yaml.yamlIndex mark),
          case context of
            "" -> problem
            _  -> context ++ ", " ++ problem
        )
  Yaml.AesonException s -> (Nothing, s)
  Yaml.OtherParseException exc -> (Nothing, show exc)
  Yaml.NonStringKeyAlias anchor value ->
    ( Nothing,
      "non-string key alias; anchor name: " ++ anchor
        ++ ", value: "
        ++ show value
    )
  Yaml.CyclicIncludes -> (Nothing, "cyclic includes")
  Yaml.LoadSettingsException _ _ -> (Nothing, "loading settings exception")
  Yaml.NonStringKey _ -> (Nothing, "non string key")
  Yaml.MultipleDocuments -> (Nothing, "multiple documents")
#endif

emptyIspSpan :: Isp
emptyIspSpan = IspSpan 0 ""

normalizeListItems :: NonEmpty [Block Isp] -> NonEmpty [Block Isp]
normalizeListItems xs' =
  if getAny $ foldMap (foldMap (Any . isParagraph)) (drop 1 x :| xs)
    then fmap toParagraph <$> xs'
    else case x of
      []       -> xs'
      (y : ys) -> r $ (toNaked y : ys) :| xs
  where
    (x :| xs) = r xs'
    r = NE.reverse . fmap reverse
    isParagraph = \case
      OrderedList _ _ -> False
      UnorderedList _ -> False
      Naked _ -> False
      _ -> True
    toParagraph (Naked inner) = Paragraph inner
    toParagraph other         = other
    toNaked (Paragraph inner) = Naked inner
    toNaked other             = other

succeeds :: (Alternative m) => m () -> m Bool
succeeds m = True <$ m <|> pure False

prependErr :: Int -> MMarkErr -> [Block Isp] -> [Block Isp]
prependErr o custom blocks = Naked (IspError err) : blocks
  where
    err = FancyError o (Set.singleton $ ErrorCustom custom)

mailtoScheme :: URI.RText 'URI.Scheme
mailtoScheme = fromJust (URI.mkScheme "mailto")

toNesTokens :: Text -> NonEmpty Char
toNesTokens = NE.fromList . T.unpack

unexpEic :: (MonadParsec e Text m) => ErrorItem Char -> m a
unexpEic x =
  failure
    (Just x)
    (Set.singleton . Label . NE.fromList $ "inline content")

nes :: a -> NonEmpty a
nes a = a :| []

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ =
  error "Text.MMark.Parser.fromRight: the impossible happened"

bakeText :: (String -> String) -> Text
bakeText = T.pack . reverse . ($ [])

-- | Report custom failure at specified location.
customFailure' ::
  (MonadParsec MMarkErr Text m) =>
  Int ->
  MMarkErr ->
  m a
customFailure' o e =
  parseError $
    FancyError
      o
      (Set.singleton (ErrorCustom e))
