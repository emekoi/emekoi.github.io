module Blog.Util
  ( ExtensionT
  , liftAction
  , titleSlug
  , highlight
  , demoteHeaders
  , rawBlocks
  , descriptionList
  ) where

import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString       as BS
import qualified Data.Char             as Char
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Data.Text.ICU         as I
import qualified Data.Text.ICU.Char    as I
import           Development.Shake
import           GHC.SyntaxHighlighter (Token (..), tokenizeHaskell)
import           Lucid
import           Text.MMark.Extension  as MMark
import           Text.MMark.Type       as MMark

latin1Replace :: Char -> Maybe Text
latin1Replace '\x0c6' = Just "ae"
latin1Replace '\x0e6' = Just "ae"
latin1Replace '\x0d0' = Just "d"
latin1Replace '\x0f0' = Just "d"
latin1Replace '\x0d8' = Just "o"
latin1Replace '\x0f8' = Just "o"
latin1Replace '\x0de' = Just "th"
latin1Replace '\x0fe' = Just "th"
latin1Replace '\x0df' = Just "ss"
latin1Replace '\x131' = Just "i"
latin1Replace '\x141' = Just "oe"
latin1Replace '\x142' = Just "oe"
latin1Replace '\x152' = Just "oe"
latin1Replace '\x153' = Just "oe"
latin1Replace '\x191' = Just "f"
latin1Replace '\x192' = Just "f"
latin1Replace _       = Nothing

titleSlug :: Text -> Text
titleSlug = f . I.nfd
  where
    g c | I.property I.Diacritic c = ""
    g c | Char.isAscii c && Char.isAlphaNum c = T.singleton $ Char.toLower c
    g c | Just c' <- latin1Replace c = c'
    g _ = " "
    f = T.intercalate "-"
      . T.words
      . T.concatMap g

-- | convert an Action r to a Rules (Action r) with sharing
liftAction :: Action a -> Rules (Action a)
liftAction m = ($ ()) <$> newCache (`seq` m)

-- scuffed implementation of description lists
descriptionList :: MonadFail m => ExtensionT m
descriptionList = blockRenderM \old -> \case
  Div attrs blocks | elem "dl" (MMark.classes attrs) -> do
    dl_ $ forM_ (pairUp blocks) \(x, y) -> do
      dt_ (old x)
      dd_ (old y)
    "\n"
  x -> old x
  where
    pairUp = reverse . fst . foldl go ([], Nothing)

    go (acc, Nothing) fst  = (acc, Just fst)
    go (acc, Just fst) snd = ((fst, snd) : acc, Nothing)

-- emit blocks with language 'raw' as raw HTML
rawBlocks :: MonadFail m => ExtensionT m
rawBlocks = blockRenderM \old -> \case
  CodeBlock (Just "{=raw}") txt -> toHtmlRaw txt
  x -> old x

-- demote headers by 1 (h1 -> h2, ..., h6 -> p)
demoteHeaders :: MonadFail m => ExtensionT m
demoteHeaders = blockTransM \case
  Heading1 x -> pure $ Heading2 x
  Heading2 x -> pure $ Heading3 x
  Heading3 x -> pure $ Heading4 x
  Heading4 x -> pure $ Heading5 x
  Heading5 x -> pure $ Heading6 x
  Heading6 x -> pure $ Paragraph x
  x -> pure x

ghcHighlight :: Monad m => Text -> Maybe (HtmlT m ())
ghcHighlight (tokenizeHaskell -> Just x) = pure $
  forM_ x \(c, toHtml -> h) ->
    case tokenClass c of
      Just c  -> span_ [class_ c] h
      Nothing -> span_ h
  where
    tokenClass :: Token -> Maybe Text
    tokenClass = \case
      KeywordTok     -> Just "k"
      PragmaTok      -> Just "na"
      SymbolTok      -> Just "p"
      VariableTok    -> Just "nv"
      ConstructorTok -> Just "kt"
      OperatorTok    -> Just "o"
      CharTok        -> Just "sc"
      StringTok      -> Just "s2"
      IntegerTok     -> Just "m"
      RationalTok    -> Just "mf"
      CommentTok     -> Just "c"
      SpaceTok       -> Nothing
      OtherTok       -> Just "x"
ghcHighlight _ = Nothing

highlight :: ExtensionT Action
highlight = blockRenderM \old block -> case block of
  CodeBlock (Just "haskell") txt ->
    case ghcHighlight txt of
      Just html -> wrap html
      Nothing   -> old block
  CodeBlock (Just l) txt -> do
    html <- lift $ pygmentize l (T.encodeUtf8 txt)
    wrap $ toHtmlRaw html
  _ -> old block
 where
   pygmentize :: Text -> BS.ByteString -> Action BS.ByteString
   pygmentize lang raw = fromStdout <$> cmd @(CmdOption -> [String] -> Action _)
     (StdinBS (BS.fromStrict raw))
     [ "pygmentize", "-l", T.unpack lang, "-f", "html", "-O", "nowrap=True"]

   wrap :: Monad m => HtmlT m () -> HtmlT m ()
   wrap x = div_ [class_ "hl"] ("\n" <* pre_ x)
