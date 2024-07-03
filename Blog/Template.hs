module Blog.Template
    ( module Blog.Template
    ) where

import qualified Blog.Config           as C
import           Blog.MMark            (Document (..))
import           Control.Exception
import           Data.Aeson            (Value (..), (.=))
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
  meta <- C.baseMetadata
  either (liftIO . throwIO . MustacheParserException) return do
    nodes <- Stache.parseMustache file input
    pure . TextL.toStrict $
      Stache.renderMustache (Template pname [(pname, nodes)]) (Object meta)
  where pname = Stache.PName $ Text.pack file

render :: Template -> Document -> Action Document
render t doc@(Doc meta body) = do
  base <- C.baseMetadata
  pure doc
    { meta = base <> meta <> ("body" .= TextL.toStrict body)
    , body = Stache.renderMustache t (Object mempty)
    }
