{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Blog.Template
  ( Template (..)
  , compileDir
  , empty
  , preprocess
  , preprocessFile
  , renderPage
  ) where

import           Blog.Type
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                 (Value (..), (.=))
import qualified Data.Aeson                 as Aeson
import           Data.Foldable
import           Data.Function              (fix)
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
import           Development.Shake.Classes
import           Development.Shake.FilePath ((<.>), (</>))
import qualified Text.Megaparsec.Error      as Mega
import qualified Text.Megaparsec.Pos        as Mega
import qualified Text.Mustache              as Stache
import qualified Text.Mustache.Parser       as Stache
import           Text.Mustache.Type         (Node (..), Template (..))

instance NFData Node
instance NFData Template

instance Binary Mega.Pos
instance Binary Stache.Key
instance Binary Node
instance Binary Stache.PName
instance Binary Template

instance Hashable Mega.Pos
instance Hashable Stache.Key
instance Hashable Node
instance Hashable Stache.PName
instance Hashable Template

newtype TemplateQ = TemplateQ FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

newtype TemplateA = TemplateA Template
  deriving (Typeable, Eq, Hashable, NFData, Binary)

instance Show TemplateA where
  show = const "Template{..}"

type instance RuleResult TemplateQ = TemplateA

templateAnswer :: TemplateA -> Template
templateAnswer (TemplateA t) = t

empty :: Template
empty = Template undefined mempty

compileDir :: FilePath -> Rules (FilePath -> Action Template)
compileDir dir = fmap wrap . addOracleCache $ fix \loop (TemplateQ tName) -> do
  let
    mInputFile = dir </> tName <.> "mustache"
    pName = Stache.PName (Text.pack tName)

  putInfo $ unwords ["TEMPLATE", tName]

  need [mInputFile]
  mInput <- liftIO $ Text.readFile mInputFile

  case Stache.parseMustache tName mInput of
    Left err -> fileError (Just mInputFile) $
      Mega.errorBundlePretty err
    Right nodes -> do
      -- template [Text.unpack $ Stache.unPName p | Partial p _ <- nodes]
      ts <- forP (snd $ foldl' getPartials (mempty, []) nodes) (wrap loop)
      pure . TemplateA $
        sconcat (Template pName (Map.singleton pName nodes) :| ts)
  where
    wrap x = fmap templateAnswer . x . TemplateQ
    getPartials (c, r) (Partial p _)
      | p `Set.notMember` c = (Set.insert p c, Text.unpack (Stache.unPName p) : r)
    getPartials acc _ = acc

preprocess :: Aeson.Value -> Template -> Maybe FilePath -> Text -> Action Text
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
