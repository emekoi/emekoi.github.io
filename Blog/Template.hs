module Blog.Template
  ( Template
  , compileDir
  , preprocess
  , preprocessFile
  , renderPage
  , renderPost
  ) where

import           Blog.MMark                 (Page (..))
import           Blog.Shake
import           Blog.Util
import           Control.Applicative
import           Control.Exception
import           Control.Monad.IO.Class
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

compileDir :: FilePath -> Rules (Text -> Action (Maybe Template))
compileDir dir = do
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
    needTemplates (Text.unpack . Stache.unPName <$> Set.toList deps)
    pure . Just $ Template pname (Map.restrictKeys cache deps)

  where
    getPartials cache !r (Partial n _ : xs) =
      let r' = Set.insert n r
        in getPartials cache (getPartials cache r' (cache Map.! n)) xs
    getPartials cache !r (_ : xs) = getPartials cache r xs
    getPartials _ !r [] = r

    needTemplates = need . fmap (\x -> dir </> x <.> ".mustache")

preprocess :: (MonadFail m, MonadIO m) => Aeson.Value -> Template -> Text -> m Text
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

renderPage :: Aeson.Value -> Template -> Page -> TextL.Text
renderPage site t (Page meta body) = do
  Stache.renderMustache t . Object $ meta
    <> ("site" .= site)
    <> ("body" .= TextL.toStrict body)

renderPost :: (MonadFail m) => Aeson.Value -> Template -> Page -> m (Post, TextL.Text)
renderPost site t p@(Page meta body) = do
  (Object fallback) <- pure . Aeson.toJSON $ emptyPost { body = body }

  case Aeson.fromJSON (Object $ meta <> fallback) of
    Aeson.Error err                     -> fail err
    Aeson.Success v | Text.null v.title -> fail "missing metadata field: title"
    Aeson.Success Post{..}              ->
      let
        post = Post
          { publishedIs8601 = Nothing
          , updated         = updated <|> published
          , updatedIso8601  = Nothing
          , slug            = slug <|> Just (titleSlug title)
          , ..
          }
      in pure (post, renderPage site t p)
