module Blog.Template
    ( Template (..)
    , compileDir
    , preprocess
    , preprocessFile
    , renderPage
    ) where

import Blog.Type
import Control.Concurrent.MVar    qualified as MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson                 (Value (..), (.=))
import Data.Aeson                 qualified as Aeson
import Data.Map.Strict            qualified as Map
import Data.Maybe
import Data.Set                   qualified as Set
import Data.Text                  qualified as Text
import Data.Text.IO               qualified as Text
import Data.Text.Lazy             qualified as TextL
import Development.Shake
import Development.Shake.FilePath ((<.>), (</>))
import Development.Shake.FilePath qualified as Shake
import Text.Megaparsec.Error      qualified as Mega
import Text.Mustache              qualified as Stache
import Text.Mustache.Parser       qualified as Stache
import Text.Mustache.Type         (Node (..), Template (..))

compileDir :: FilePath -> Rules (FilePath -> Action Template)
compileDir dir = do
  templateMap <- liftIO $ MVar.newMVar mempty

  dir </> "*.mustache" %> \mInputFile -> do
    let
      tName = Shake.takeBaseName mInputFile
      pName = Stache.PName (Text.pack tName)

    putInfo $ unwords ["TEMPLATE", tName]

    mInput <- liftIO $ Text.readFile mInputFile

    case Stache.parseMustache tName mInput of
      Left err -> fileError (Just mInputFile) $
        Mega.errorBundlePretty err
      Right nodes -> do
        let partials = foldMap getPartials nodes

        need . fmap (getInputFile . Text.unpack . Stache.unPName) $
          Set.toList partials

        liftIO $ MVar.modifyMVar_ templateMap \ts ->
          let ts' = Map.insert pName nodes ts
          in pure ts'

  pure $ \tName -> do
    need [dir </> tName <.> "mustache"]
    Template (Stache.PName (Text.pack tName))
      <$> liftIO (MVar.readMVar templateMap)
  where
    getInputFile tName = dir </> tName <.> "mustache"
    getPartials (Partial p _) = Set.singleton p
    getPartials _             = mempty

preprocess :: Aeson.Value -> Template -> Maybe FilePath -> StrictText -> Action StrictText
preprocess site (Template _ tc) file input = do
  (ws, out) <- either throw return do
    nodes <- Stache.parseMustache fname input
    pure . fmap TextL.toStrict $ Stache.renderMustacheW
      (Template pname (Map.insert pname nodes tc))
      (Object $ "site" .= site)
  unless (null ws) $
    putInfo . unlines $ (fmtWarning <$> ws)
  pure out
  where
    fmtWarning x = fname <> ": " <> Stache.displayMustacheWarning x
    throw = fileError file . Mega.errorBundlePretty
    pname = Stache.PName $ Text.pack fname
    fname = fromMaybe "<input>" file

preprocessFile :: Aeson.Value -> Template -> FilePath -> Action StrictText
preprocessFile site t file = do
  need [file]
  input <- liftIO $ Text.readFile file
  preprocess site t (Just file) input

renderPage :: Aeson.Value -> Template -> Page -> TextL.Text
renderPage site t (Page meta body) = do
  Stache.renderMustache t . Object $ ("site" .= site)
    <> ("body" .= TextL.toStrict body)
    <> meta
