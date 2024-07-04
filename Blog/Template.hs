module Blog.Template
    ( module Blog.Template
    ) where

import           Blog.MMark            (Page (..))
import           Blog.Shake
import           Control.Exception
import           Data.Aeson            (Value (..), (.=))
import qualified Data.Aeson            as Aeson
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.Map.Strict       as Map
import           Data.Semigroup
import qualified Data.Set              as Set
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Lazy        as TextL
import           Development.Shake
import qualified Text.Mustache         as Stache
import qualified Text.Mustache.Compile as Stache
import qualified Text.Mustache.Parser  as Stache
import           Text.Mustache.Type    (MustacheException (..), Node (..),
                                        Template (..))

compile :: Stache.PName -> FilePath -> Action (Maybe Template)
compile pname dir = do
  files <- Stache.getMustacheFilesInDir dir
  case files of
    []   -> pure Nothing
    x : xs -> do
      t  <- Stache.compileMustacheFile x
      ts <- forP xs Stache.compileMustacheFile
      let
        cache = Stache.templateCache $ sconcat (t :| ts)
        names = [Text.unpack $ Stache.unPName pname]
      need . Set.toList $ getPartials cache names (cache Map.! pname)
      pure . Just $ Template pname cache
  where
    getPartials cache !r (Partial n _ : xs) =
      let r' = Set.insert (Text.unpack $ Stache.unPName n) r
        in getPartials cache (getPartials cache r' (cache Map.! n)) xs
    getPartials cache !r (_ : xs) = getPartials cache r xs
    getPartials _ !r [] = r

preprocess :: FilePath -> Action Text
preprocess file = do
  need [file]
  input <- liftIO $ Text.readFile file
  siteMeta <- Aeson.toJSON <$> defaultSite
  either (liftIO . throwIO . MustacheParserException) return do
    nodes <- Stache.parseMustache file input
    pure . TextL.toStrict $
      Stache.renderMustache (Template pname [(pname, nodes)]) (Object $ "site" .= siteMeta)
  where pname = Stache.PName $ Text.pack file

renderPage :: Template -> Page -> Action TextL.Text
renderPage t (Page meta body) = do
  siteMeta <- Aeson.toJSON <$> defaultSite
  pure . Stache.renderMustache t . Object $ meta
    <> ("site" .= siteMeta)
    <> ("body" .= TextL.toStrict body)

renderPost :: Template -> Page -> Action (Post, TextL.Text)
renderPost t p@(Page meta body) = do
  (Object fallback) <- pure . Aeson.toJSON $ Post
    { body      = body
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
