module Utils
    ( module Utils
    ) where

import Config                          qualified as C
import Control.Applicative
import Control.Monad
import Data.Text                       (Text)
import Data.Text                       qualified as T
import Data.Text.Lazy                  qualified as TL
import Data.Time.Clock
import Data.Time.Format
import GHC.SyntaxHighlighter           (Token (..), tokenizeHaskell)
import Hakyll                          hiding (defaultContext, pandocCompiler,
                                        tagsField)
import Hakyll                          qualified as H
import Hakyll.Core.Compiler.Internal
import System.FilePath                 (takeFileName)
import System.Process
import Text.Blaze.Html.Renderer.String qualified as H
import Text.Blaze.Html.Renderer.Text   (renderHtml)
import Text.Blaze.Html5                qualified as H
import Text.Blaze.Html5.Attributes     qualified as A
import Text.Pandoc
import Text.Pandoc.Shared              (headerShift)
import Text.Pandoc.Walk

titleSlug :: String -> IO String
titleSlug = readProcess "scripts/title-slug" [] . takeFileName

basename :: Routes
basename = customRoute (takeFileName . toFilePath)

defaultContext :: Context String
defaultContext =
    foldMap (uncurry constField) fields
    <> field "git-hash" (const gitHash)
    <> H.defaultContext
  where
    fields =
      [ ("author", C.author)
      , ("lang", C.siteLang)
      , ("github", C.github)
      , ("site-source", C.siteSource)
      , ("site-title", C.siteTitle)
      , ("site-url", C.siteURL)
      , ("email", C.email)
      ]

parseDate :: String -> Maybe UTCTime
parseDate = parseTimeM @_ @UTCTime True defaultTimeLocale "%Y-%m-%d"

postCtx :: Context String
postCtx =
    dateField "published" "%Y-%m-%d"
    <> functionField "date" fmtDate
    <> defaultContext
    <> tagsField
  where
    fmtDate ((parseDate -> Just date ):xs) _ = do
      let fmt = case xs of [] -> "%e %B %Y"; fmt : _ -> fmt
      pure $ formatTime defaultTimeLocale fmt date
    fmtDate _ _ = error "invalid use of date function"

tagsField :: Context a
tagsField = field "tags" \(Item id _) -> do
  meta <- getMetadata id
  case lookupStringList "tags" meta of
    Nothing -> empty
    Just tags ->
      pure . H.renderHtml . H.toHtml $ f . ('#':) <$> tags
  where
    f x = H.li $ H.a
        H.! A.href (H.toValue $ "/tags.html" ++ x)
        H.! A.rel "tag" $ H.toHtml x

ghcHighlight :: Text -> Maybe H.Html
ghcHighlight (tokenizeHaskell -> Just x) =
  pure $ formatTokens x
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

    formatTokens :: [(Token, Text)] -> H.Html
    formatTokens = mapM_ \(c, H.toHtml -> h) ->
      case tokenClass c of
        Just c  -> H.span H.! A.class_ (H.toValue c) $ h
        Nothing -> H.span h
ghcHighlight _ = Nothing

highlight :: Pandoc -> Compiler Pandoc
highlight = walkM \case
  (CodeBlock (_, "haskell" : _, _) (ghcHighlight -> Just body)) ->
    pure $ RawBlock "html" (wrap body)
  CodeBlock (_, (T.unpack -> lang) : _, _) (T.unpack -> body) ->
    RawBlock "html" . wrap <$> unsafeCompiler (pygs lang body)
  block -> pure block
 where
   pygs :: String -> String -> IO String
   pygs lang = readProcess "pygmentize" ["-l", lang, "-f", "html", "-O", "nowrap=True"]

   wrap :: (H.ToMarkup a) => a -> T.Text
   wrap = TL.toStrict . renderHtml
     . (H.div H.! A.class_ "hl")
     . H.pre . H.preEscapedToMarkup

pandocCCPre :: (Item String -> Compiler (Item String)) -> Compiler (Item String)
pandocCCPre f =
  getResourceBody >>= f >>= renderPandocWithTransformM reader writer
    (foldl1 (>=>) passes)
  where
    passes :: [Pandoc -> Compiler Pandoc]
    passes = [pure . headerShift 1, highlight]

    reader :: ReaderOptions
    reader = defaultHakyllReaderOptions
      { readerStripComments = True
      }

    writer :: WriterOptions
    writer = defaultHakyllWriterOptions
      { writerHTMLMathMethod = KaTeX ""
      , writerWrapText = WrapNone
      , writerEmailObfuscation = JavascriptObfuscation
      , writerHtmlQTags = True
      , writerHighlightStyle = Nothing
      }

pandocCC :: Compiler (Item String)
pandocCC = pandocCCPre pure

gitHash :: Compiler String
gitHash = compilerUnsafeIO $
  readProcess "git" ["rev-parse", "--short", "master"] []
