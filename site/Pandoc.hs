module Pandoc
    ( pandocCompiler
    , pandocCompiler'
    , pandocCompilerM
    ) where

import Data.Text                       (Text)
import Data.Text.Lazy                  qualified as TL
import GHC.SyntaxHighlighter           (Token (..), tokenizeHaskell)
import Hakyll (defaultHakyllReaderOptions, defaultHakyllWriterOptions, unixFilter)
import Text.Blaze.Html.Renderer.Text   (renderHtml)
import Text.Blaze.Html5                qualified as H
import Text.Blaze.Html5.Attributes     qualified as A
import Text.Pandoc.Shared              (headerShift)
import Text.Pandoc.Walk
import Control.Monad
import Data.Text                  qualified as T
import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Hakyll.Web.Pandoc.FileType
import Text.Pandoc

pandocIO :: PandocIO a -> Compiler (Either PandocError a)
pandocIO = unsafeCompiler . runIO

readPandocWith
  :: ReaderOptions
  -> Item String
  -> Compiler (Item Pandoc)
readPandocWith ropt i@(Item id body) =
    pandocIO (reader ropt (itemFileType i) (T.pack body)) >>= \case
    Left err    -> fail $
        "Hakyll.Web.Pandoc.readPandocWith: parse failed: " ++ show err
    Right item' -> pure . Item id $ item'
  where
    reader ro t = case t of
      DocBook            -> readDocBook ro
      Html               -> readHtml ro
      Jupyter            -> readIpynb ro
      LaTeX              -> readLaTeX ro
      LiterateHaskell t' -> reader ro t'
      Markdown           -> readMarkdown ro
      MediaWiki          -> readMediaWiki ro
      OrgMode            -> readOrg ro
      Rst                -> readRST ro
      Textile            -> readTextile ro
      _                  -> error $
        "Hakyll.Web.readPandocWith: I don't know how to read a file of " ++
        "the type " ++ show t ++ " for: " ++ show id

writePandocWith
  :: WriterOptions
  -> Item Pandoc
  -> Compiler (Item String)
writePandocWith wopt (Item id body) =
  pandocIO (writeHtml5String wopt body) >>= \case
    Left err    -> fail $ "Hakyll.Web.Pandoc.writePandocWith: " ++ show err
    Right body -> pure . Item id $ T.unpack body

renderPandocM
  :: ReaderOptions
  -> WriterOptions
  -> (Pandoc -> Compiler Pandoc)
  -> Item String
  -> Compiler (Item String)
renderPandocM ropt wopt f =
  readPandocWith ropt >=> traverse f >=> writePandocWith wopt

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

pandocCompiler' :: (Item String -> Compiler (Item String)) -> Compiler (Item String)
pandocCompiler' f =
  getResourceBody >>= f >>= renderPandocM reader writer
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

pandocCompiler :: Compiler (Item String)
pandocCompiler = pandocCompiler' pure

pandocCompilerM
  :: ReaderOptions
  -> WriterOptions
  -> (Pandoc -> Compiler Pandoc)
  -> Compiler (Item String)
pandocCompilerM ropt wopt f = getResourceBody >>= renderPandocM ropt wopt f
