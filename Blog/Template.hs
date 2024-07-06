
module Blog.Template
  ( Template
  , compileDir
  , preprocess
  , preprocessFile
  , renderPage
  , renderPost
  ) where

import           Blog.Type
import           Blog.Util
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                 (Value (..), (.=))
import qualified Data.Aeson                 as Aeson
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import qualified Data.Text.Lazy             as TextL
import           Development.Shake
import           Development.Shake.FilePath ((<.>), (</>))
import qualified Text.Megaparsec.Error      as Mega
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

preprocess :: Aeson.Value -> Template -> Maybe FilePath -> Text -> Action Text
preprocess site (Template _ tc) file input = do
  (ws, out) <- either throw return do
    nodes <- Stache.parseMustache "<input>" input
    pure . fmap TextL.toStrict $ Stache.renderMustacheW
      (Template pname (Map.insert pname nodes tc))
      (Object $ "site" .= site)
  unless (null ws) $
    liftIO . putStrLn $ unlines (Stache.displayMustacheWarning <$> ws)
  pure out
  where
    throw = liftIO . throwIO . FileError file . Mega.errorBundlePretty
    pname = Stache.PName $ Text.pack fname
    fname = fromMaybe "<input>" file

preprocessFile :: Aeson.Value -> Template -> FilePath -> Action Text
preprocessFile site t file = do
  need [file]
  input <- liftIO $ Text.readFile file
  preprocess site t (Just file) input

renderPage :: Aeson.Value -> Template -> Page -> TextL.Text
renderPage site t (Page meta body) = do
  Stache.renderMustache t . Object $ ("site" .= site)
    <> ("body" .= TextL.toStrict body)
    <> meta

renderPost :: (MonadFail m, MonadIO m) => Aeson.Value -> Template -> Page -> m (Post, TextL.Text)
renderPost site t page = do
  case Aeson.fromJSON (Object page.meta) of
    Aeson.Error err                     -> fail err
    Aeson.Success v | Text.null v.title -> fail "missing metadata field: title"
    Aeson.Success Post{..}              ->
      let meta = "tags" .= linkifyTags tags <> page.meta in
      pure
        ( Post { body = page.body, .. }
        , renderPage site t (Page meta page.body)
        )
