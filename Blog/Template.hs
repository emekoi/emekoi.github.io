module Blog.Template
  ( compileTemplates
  , preprocess
  , preprocessWith
  , renderPage
  , renderPageWith
  , renderPost
  ) where

import           Blog.MMark                 (Page (..))
import           Blog.Shake
import           Control.Exception
import           Data.Aeson                 (Value (..), (.=))
import qualified Data.Aeson                 as Aeson
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.Map.Strict            as Map
import           Data.Semigroup
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import qualified Data.Text.Lazy             as TextL
import           Development.Shake
import           Development.Shake.FilePath ((<.>), (</>))
import qualified Text.Mustache              as Stache
import qualified Text.Mustache.Compile      as Stache
import qualified Text.Mustache.Parser       as Stache
import           Text.Mustache.Type         (Node (..), Template (..))

compileTemplates :: FilePath -> Rules (Text -> Action (Maybe Template))
compileTemplates dir = newCache $ \(Stache.PName -> pname) -> do
  putInfo "Compiling Templates"
  files <- Stache.getMustacheFilesInDir dir
  case files of
    []     ->  pure Nothing
    x : xs -> do
      t  <- Stache.compileMustacheFile x
      ts <- forP xs Stache.compileMustacheFile
      let
        cache = Stache.templateCache $ sconcat (t :| ts)
        names = Set.singleton $ Text.unpack $ Stache.unPName pname
      needTemplates . Set.toList $ getPartials cache names (cache Map.! pname)
      pure . Just $ Template pname cache
  where
    getPartials cache !r (Partial n _ : xs) =
      let r' = Set.insert (Text.unpack $ Stache.unPName n) r
        in getPartials cache (getPartials cache r' (cache Map.! n)) xs
    getPartials cache !r (_ : xs) = getPartials cache r xs
    getPartials _ !r [] = r

    needTemplates = need . fmap (\x -> dir </> x <.> ".mustache")

preprocessWith :: FilePath -> Aeson.Object -> Action Text
preprocessWith file meta = do
  need [file]
  input <- liftIO $ Text.readFile file
  siteMeta <- Aeson.toJSON <$> defaultSite
  either (liftIO . throwIO . Stache.MustacheParserException) return do
    nodes <- Stache.parseMustache file input
    pure . TextL.toStrict $
      Stache.renderMustache (Template pname [(pname, nodes)]) (Object $ meta <> "site" .= siteMeta)
  where pname = Stache.PName $ Text.pack file

preprocess :: FilePath -> Action Text
preprocess file = preprocessWith file mempty

renderPageWith :: Template -> Page -> Aeson.Object -> Action TextL.Text
renderPageWith t (Page meta body) meta' = do
  siteMeta <- Aeson.toJSON <$> defaultSite
  pure . Stache.renderMustache t . Object $ (meta' <> meta)
    <> ("site" .= siteMeta)
    <> ("body" .= TextL.toStrict body)

renderPage :: Template -> Page -> Action TextL.Text
renderPage t p = renderPageWith t p mempty

renderPost :: Template -> Page -> Action (Post, TextL.Text)
renderPost t p@(Page meta body) = do
  (Object fallback) <- pure . Aeson.toJSON $ Post
    { body      = body
    , direction = Nothing
    , hideTitle = False
    , published = Nothing
    , subtitle  = Nothing
    , tags      = mempty
    , title     = error $ "missing metadata field: title"
    , updated   = Nothing
    , url       = Nothing
    }

  case Aeson.fromJSON (Object $ meta <> fallback) of
    Aeson.Error err -> fail err
    Aeson.Success v -> (v, ) <$> renderPage t p
