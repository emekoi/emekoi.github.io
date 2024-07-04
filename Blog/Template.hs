module Blog.Template
  ( compileTemplates
  , preprocess
  , preprocessFile
  , renderPage
  , renderPost
  ) where

import           Blog.MMark                 (Page (..))
import           Blog.Shake
import           Blog.Util
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
compileTemplates dir = do
  getCache <- liftAction do
    putInfo "Compiling Templates"
    files <- Stache.getMustacheFilesInDir dir
    case files of
      []     -> pure Map.empty
      x : xs -> do
        t  <- Stache.compileMustacheFile x
        ts <- forP xs Stache.compileMustacheFile
        pure . Stache.templateCache $ sconcat (t :| ts)

  pure $ \(Stache.PName -> pname) -> do
    cache <- getCache
    let
      names = Set.singleton pname
      deps = getPartials cache names (cache Map.! pname)
    needTemplates ((Text.unpack . Stache.unPName) <$> Set.toList deps)
    pure . Just $ Template pname (Map.restrictKeys cache deps)

  where
    getPartials cache !r (Partial n _ : xs) =
      let r' = Set.insert n r
        in getPartials cache (getPartials cache r' (cache Map.! n)) xs
    getPartials cache !r (_ : xs) = getPartials cache r xs
    getPartials _ !r [] = r

    needTemplates = need . fmap (\x -> dir </> x <.> ".mustache")

preprocess :: Aeson.Value -> Template -> Text -> Action Text
preprocess site (Template _ tc) input = do
  either (liftIO . throwIO . Stache.MustacheParserException) return do
    nodes <- Stache.parseMustache "<input>" input
    pure . TextL.toStrict $ Stache.renderMustache
      (Template pname (Map.insert pname nodes tc))
      (Object $ "site" .= site)
  where pname = Stache.PName $ Text.pack "<input>"

preprocessFile :: Aeson.Value -> Template -> FilePath -> Action Text
preprocessFile site t file = do
  need [file]
  input <- liftIO $ Text.readFile file
  preprocess site t input

renderPage :: Aeson.Value -> Template -> Page -> Action TextL.Text
renderPage site t (Page meta body) = do
  pure . Stache.renderMustache t . Object $ meta
    <> ("site" .= site)
    <> ("body" .= TextL.toStrict body)

renderPost :: Aeson.Value -> Template -> Page -> Action (Post, TextL.Text)
renderPost site t p@(Page meta body) = do
  (Object fallback) <- pure . Aeson.toJSON $ Post
    { body            = body
    , direction       = Nothing
    , hideTitle       = False
    , published       = Nothing
    , publishedIs8601 = Nothing
    , subtitle        = Nothing
    , tags            = mempty
    , title           = mempty
    , updated         = Nothing
    , updatedIso8601  = Nothing
    , url             = Nothing
    }

  case Aeson.fromJSON (Object $ meta <> fallback) of
    Aeson.Error err                     -> fail err
    Aeson.Success v | Text.null v.title -> fail "missing metadata field: title"
    Aeson.Success v                     -> (v, ) <$> renderPage site t p
