{-# LANGUAGE QuasiQuotes #-}

module Blog.Agda
    ( readAgda
    , readAgdaNaked
    ) where

import Blog.Type
import Control.Monad
import Control.Monad.IO.Class
import Data.Char                  qualified as Char
import Data.Foldable              (foldl')
import Data.Text                  qualified as Text
import Data.Text.Lazy             qualified as TextL
import Data.Text.Lazy.Builder
import Data.Void                  (Void)
import Development.Shake.FilePath ((<.>))
import Development.Shake.FilePath qualified as Shake
import Lens.Micro
import Prelude                    hiding (concatMap, readFile)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.URI                   qualified as URI (URI (uriAuthority), parser,
                                                    render')
import Text.URI.Lens              qualified as URI
import Text.URI.QQ                (pathPiece)

data Agda
  = Space StrictText
  | Anchor
  { id     :: StrictText
  , href   :: Maybe URI.URI
  , inner  :: StrictText
  , class_ :: Maybe StrictText
  }
  deriving (Show)

data Doc
  = Empty
  | Markup Builder
  | Code [Agda]
  deriving (Show)

pLine :: Parsec Void StrictText StrictText
pLine = takeWhile1P Nothing (/= '\n')

pAgda :: Parsec Void StrictText Agda
pAgda = Space <$> takeWhile1P (Just "whitespace") Char.isSpace
  <|> label "HTML anchor" do
    chunk' "<a"
    id <- chunk' "id=\"" *> word "anchor indentifier" <* chunk' "\""
    href <- optional do
      chunk' "href=\"" *> label "anchor target" URI.parser <* chunk' "\""
    class_ <- optional do
      chunk' "class=\"" *> word "anchor class" <* chunk' "\""
    inner <- single '>' *> takeWhileP (Just "inner HTML") (/= '<')
    chunk "</a>"
    pure Anchor {..}
  where
    word t = takeWhile1P (Just t) (/= '"')
    chunk' x = Text.Megaparsec.chunk x <* hspace

pCode :: Parsec Void StrictText Doc
pCode = label "Agda HTML" do
  void $ chunk "<pre class=\"Agda\">"
  Code <$> manyTill
    pAgda
    (chunk "</pre>" <* optional (single '\n'))

pMarkup :: Parsec Void StrictText Doc
pMarkup = Markup . fromText <$>
  label "markup" (pLine <* single '\n')

pEmpty :: Parsec Void StrictText Doc
pEmpty = Empty <$ single '\n'

renderDoc :: Builder -> Builder -> StrictText -> [Doc] -> LazyText
renderDoc agdaStart agdaEnd name lines =
  toLazyText $ foldl' goDoc mempty lines
  where
    goDoc acc Empty      = acc <> "\n"
    goDoc acc (Markup b) = acc <> b <> "\n"
    goDoc acc (Code [])  = acc <> "\n"
    goDoc acc (Code xs)  = acc
      <> foldl' (\acc x -> acc <> goAgda x) agdaStart xs
      <> agdaEnd

    goAgda (Space x) = fromText x
    goAgda Anchor{..} = mconcat
      [ "<a"
      , pair "id" (fromText id)
      , maybe mempty (pair "href" . fixLink) href
      , maybe mempty (pair "class" . fromText) class_
      , ">"
      , fromText inner
      , "</a>"
      ]

    pair k v = " " <> k <> "=\"" <> v <> "\""

    fixLink uri =
      URI.render' case uri ^. URI.uriPath of
        [path] | path ^. URI.unRText == name ->
          uri & URI.uriPath .~ []
        [_] | not $ uri ^. URI.isPathAbsolute ->
          uri {URI.uriAuthority = Left True} & URI.uriPath %~
            (\x -> [pathPiece|static|] : [pathPiece|agda|] : x)
        _ -> uri

readDoc
  :: MonadIO m => Parsec Void StrictText Doc -> Builder -> Builder
  -> FilePath -> StrictText -> m LazyText
readDoc ps start end file source = do
  case parse (many ps <* eof) file source of
    Left errs -> fileError (Just file) $ errorBundlePretty errs
    Right r   -> pure $ renderDoc start end mName r
  where mName = Text.pack $ Shake.takeBaseName file <.> "html"

readAgda :: MonadIO m => FilePath -> StrictText -> m StrictText
readAgda file source = TextL.toStrict <$>
  readDoc (pCode <|> pMarkup <|> pEmpty) agdaStart agdaEnd file source
  where
    agdaStart = "```{=raw}\n<pre class=\"Agda\">"
    agdaEnd   = "</pre>\n```\n"

readAgdaNaked :: MonadIO m => FilePath -> StrictText -> m LazyText
readAgdaNaked = readDoc (pCode <|> pEmpty) agdaStart agdaEnd
  where
    pCode = Code <$> some pAgda

    agdaStart = "<pre class=\"Agda\">"
    agdaEnd   = "</pre>\n"
