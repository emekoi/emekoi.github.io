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
import Hakyll                          hiding (dateField, defaultContext,
                                        pandocCompiler, relativizeUrls,
                                        relativizeUrlsWith, tagsField, urlField)
import Hakyll                          qualified as H
import System.FilePath                 (takeDirectory, takeFileName)
import System.Process                  (readProcess)
import Text.Blaze.Html.Renderer.String qualified as H
import Text.Blaze.Html.Renderer.Text   (renderHtml)
import Text.Blaze.Html5                qualified as H
import Text.Blaze.Html5.Attributes     qualified as A
import Text.Pandoc
import Text.Pandoc.Shared              (headerShift)
import Text.Pandoc.Walk

basename :: Routes
basename = customRoute (takeFileName . toFilePath)

defaultContext :: Context String
defaultContext =
    foldMap (uncurry constField) fields
    <> field "git-hash" (const gitHash)
    <> functionField "date" fmtDate
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
    fmtDate ((parseDate -> Just date) : xs) _ = do
      let fmt = case xs of [] -> "%e %B %Y"; fmt : _ -> fmt
      pure $ formatTime defaultTimeLocale fmt date
    fmtDate _ _ = error "invalid use of date function"

parseDate :: String -> Maybe UTCTime
parseDate x = msum [parseTimeM True defaultTimeLocale fmt x | fmt <- formats]
  where
    formats =
      [ "%a, %d %b %Y %H:%M:%S %Z"
      , "%a, %d %b %Y %H:%M:%S"
      , "%Y-%m-%dT%H:%M:%S%Z"
      , "%Y-%m-%dT%H:%M:%S"
      , "%Y-%m-%d %H:%M:%S%Z"
      , "%Y-%m-%d %H:%M:%S"
      , "%Y-%m-%d"
      , "%d.%m.%Y"
      , "%B %e, %Y %l:%M %p"
      , "%B %e, %Y"
      , "%b %d, %Y"
      ]

dateField :: String -> String -> Context String
dateField key fmt = field key \(Item i _) -> do
  getMetadataField i key >>= \case
    Just (parseDate -> Just date) -> pure $ formatTime defaultTimeLocale fmt date
    Just _ -> noResult $ "Invalid date field '" ++ key ++ "' in context"
    _ -> noResult $ "Missing field '" ++ key ++ "' in context"

postCtx :: Context String
postCtx =
    dateField "published" "%Y-%m-%dT%H:%M:%S%Ez"
    <> urlField
    <> tagsField
    <> defaultContext

urlField :: Context a
urlField = field "url" $ \i -> do
    let id = itemIdentifier i
        empty' = fail $ "No route url found for item " ++ show id
    maybe empty' (toUrl . takeDirectory) <$> getRoute id

tagsField :: Context a
tagsField = field "tags" \(Item id _) -> do
  meta <- getMetadata id
  case lookupStringList "tags" meta of
    Nothing -> empty
    Just tags ->
      pure . H.renderHtml . H.toHtml $ f . ('#':) <$> tags
  where
    f x = H.li $ H.a
        H.! A.href (H.toValue $ "/tags" ++ x)
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
    RawBlock "html" . wrap <$> pygs lang body
  block -> pure block
 where
   pygs :: String -> String -> Compiler String
   pygs lang = unixFilter "pygmentize" ["-l", lang, "-f", "html", "-O", "nowrap=True"]

   wrap :: (H.ToMarkup a) => a -> T.Text
   wrap = TL.toStrict . renderHtml
     . (H.div H.! A.class_ "hl")
     . H.pre . H.preEscapedToMarkup

addSectionLinks :: Pandoc -> Pandoc
addSectionLinks = walk \case
  Header n attr@(idAttr, _, _) inlines ->
    Header n attr [Link nullAttr inlines ("#" <> idAttr, "")]
  block -> block

imgLazyLoad :: Pandoc -> Pandoc
imgLazyLoad = walk \case
  Image (id, cs, kv) alt target ->
    Image (id, cs, ("loading", "lazy") : kv) alt target
  inline -> inline

pandocCCPre :: (Item String -> Compiler (Item String)) -> Compiler (Item String)
pandocCCPre f =
  getResourceBody >>= f >>= renderPandocWithTransformM reader writer
    (foldl1 (>=>) passes)
  where
    passes :: [Pandoc -> Compiler Pandoc]
    passes =
      [ pure . foldl1 (.) [headerShift 1, addSectionLinks, imgLazyLoad]
      , highlight
      ]

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
gitHash = unsafeCompiler $
  readProcess "git" ["rev-parse", "--short", "master"] []

-- relativizeUrls :: Item String -> Compiler (Item String)
-- relativizeUrls item = do
--   route <- getRoute $ itemIdentifier item
--   pure $ case route of
--     Just r  -> fmap (withUrls (rel r)) item
--     Nothing -> item
--   where
--     isRel ('/' : '/' : _) = False
--     isRel ('/' : _)      = True
--     isRel _              = False
--     rel _ ('$' : x) = x
--     rel r x         = if isRel x then toSiteRoot r ++ x else x
