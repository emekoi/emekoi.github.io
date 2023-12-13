module Utils
    ( module Utils
    ) where

import Data.Text                     qualified as T
import Data.Text.Lazy                qualified as TL
import Data.Time.Clock
import Data.Time.Format
import GHC.SyntaxHighlighter         (Token (..), tokenizeHaskell)
import Hakyll                        hiding (defaultContext, pandocCompiler)
import Hakyll                        qualified as H
import Hakyll.Core.Compiler.Internal
import System.FilePath               (takeFileName)
import System.Process
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5              qualified as H
import Text.Blaze.Html5.Attributes   qualified as A
import Text.Pandoc
import Text.Pandoc.Walk

basename :: Routes
basename = customRoute (takeFileName . toFilePath)

defaultContext :: Context String
defaultContext =
    foldMap (uncurry constField) fields
    <> field "git-hash" (const gitHash)
    <> H.defaultContext
  where
    fields =
      [ ("author-meta", "Emeka Nkurumeh")
      , ("lang", "en")
      , ("github", "https://github.com/emekoi")
      , ("site-source", "https://github.com/emekoi/emekoi.github.io")
      , ("site-title", "Forgetful Functor")
      , ("site-url", "https://emekoi.github.com")
      , ("email", "email@example.com")
      ]

postCtx :: Context String
postCtx =
    dateField "published" "%Y-%m-%d"
    <> functionField "date" fmtDate
    <> defaultContext
  where
    fmtDate ((parseTimeM @_ @UTCTime True defaultTimeLocale "%Y-%m-%d" -> Just date ):xs) _ = do
      let fmt = case xs of
            []      ->  "%e %B %Y"
            fmt : _ -> fmt
      pure $ formatTime defaultTimeLocale fmt date
    fmtDate _ _ = error "invalid use of date function"

ghcHighlight :: T.Text -> Maybe T.Text
ghcHighlight (tokenizeHaskell -> Just x) =
  pure . TL.toStrict $ renderHtml (formatTokens x)
  where
    tokenClass :: Token -> Maybe T.Text
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

    formatTokens = mapM_ \(c, H.toHtml -> h) ->
      case tokenClass c of
        Just c  -> H.span H.! A.class_ (H.toValue c) $ h
        Nothing -> H.span h
ghcHighlight _ = Nothing

highlight :: Pandoc -> Compiler Pandoc
highlight = walkM \case
  block@(CodeBlock (_, "haskell" : _, _) body) ->
    pure $ case ghcHighlight body of
      Just body -> RawBlock "html" body
      Nothing   -> block
  CodeBlock (_, (T.unpack -> lang) : _, _) (T.unpack -> body) ->
    RawBlock "html" . T.pack <$> unsafeCompiler (pygs lang body)
  block -> pure block
 where
  pygs lang = readProcess "pygmentize" ["-l", lang, "-f", "html", "-O", "nowrap=True"]

pandocCCPre :: (Item String -> Compiler (Item String)) -> Compiler (Item String)
pandocCCPre f =
  cached "Main.pandocCompiler" $
      getResourceBody >>= f >>= renderPandocWithTransformM reader writer highlight
  where
    reader = defaultHakyllReaderOptions
      { readerStripComments = True
      }

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
gitHash = compilerUnsafeIO $ do
  readProcess "git" ["rev-parse", "--short", "master"] []
