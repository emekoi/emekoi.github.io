{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Blog.Template
  ( Template (..)
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
import           Development.Shake.Classes
import           Development.Shake.FilePath ((<.>), (</>))
import           Development.Shake.Rule
import qualified Text.Megaparsec.Error      as Mega
import qualified Text.Megaparsec.Pos      as Mega
import qualified Text.Mustache              as Stache
import qualified Text.Mustache.Compile      as Stache
import qualified Text.Mustache.Parser       as Stache
import           Text.Mustache.Type         (Node(..), Template(..))
import Data.Foldable
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import Data.Function (fix)

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
  deriving (Show, Typeable, Eq, Hashable, NFData, Binary)

type instance RuleResult TemplateQ = TemplateA

templateAnswer :: TemplateA -> Template
templateAnswer (TemplateA t) = t

_template :: [FilePath] -> Action [Template]
_template = fmap (fmap templateAnswer) . apply . fmap TemplateQ

_addBuiltinTemplateRule :: FilePath -> Rules ()
_addBuiltinTemplateRule dir = do
  addBuiltinRule noLint noIdentity run
  where
    -- encode' :: Binary a => a -> BS.ByteString
    -- encode' = BS.fromLazy . Binary.encode

    -- decode' :: Binary a => BS.ByteString -> a
    decode' = Binary.decode . BS.fromStrict

    getPartials (c, r) (Partial p _)
      | p `Set.notMember` c = (Set.insert p c, Text.unpack (Stache.unPName p) : r)
    getPartials acc _ = acc

    run :: BuiltinRun TemplateQ TemplateA
    -- run (TemplateQ input) old RunDependenciesSame =
    run (TemplateQ tName) oldStore mode = do
      case mode of
        RunDependenciesSame | Just old <- oldStore ->
          pure $ RunResult ChangedNothing old (decode' old)
        _ -> do
          let
            mInputFile = dir </> tName <.> "mustache"
            pName = Stache.PName (Text.pack tName)

          need [mInputFile]
          mInput <- liftIO $ Text.readFile mInputFile

          case Stache.parseMustache tName mInput of
            Left err -> liftIO . throwIO . FileError (Just mInputFile) $
              Mega.errorBundlePretty err
            Right nodes -> do
              -- template [Text.unpack $ Stache.unPName p | Partial p _ <- nodes]
              ts <- _template . snd $ foldl' getPartials (mempty, []) nodes
              let
                _t = sconcat (Template pName (Map.singleton pName nodes) :| ts)
              --   _a = TemplateA t

              pure undefined

compileDir :: FilePath -> Rules (FilePath -> Action Template)
compileDir dir = fmap (\x -> fmap templateAnswer . x . TemplateQ) . addOracle $ fix \loop (TemplateQ tName) -> do
  let
    mInputFile = dir </> tName <.> "mustache"
    pName = Stache.PName (Text.pack tName)

  need [mInputFile]
  mInput <- liftIO $ Text.readFile mInputFile

  case Stache.parseMustache tName mInput of
    Left err -> liftIO . throwIO . FileError (Just mInputFile) $
      Mega.errorBundlePretty err
    Right nodes -> do
      -- template [Text.unpack $ Stache.unPName p | Partial p _ <- nodes]
      let partials = (snd $ foldl' getPartials (mempty, []) nodes)
      ts <- forP partials (fmap templateAnswer . loop . TemplateQ)

      -- liftIO $ print (sconcat (Template pName (Map.singleton pName nodes) :| ts))

      pure . TemplateA $ sconcat (Template pName (Map.singleton pName nodes) :| ts)
  where
    getPartials (c, r) (Partial p _)
      | p `Set.notMember` c = (Set.insert p c, Text.unpack (Stache.unPName p) : r)
    getPartials acc _ = acc

-- compileDir :: FilePath -> Rules (Text -> Action (Maybe Template))
-- compileDir dir = do
--   action $ putInfo "Compiling Templates"
--   files <- Stache.getMustacheFilesInDir dir
-- #if defined(TEMPLATE_CACHE)
--   getCache <- liftAction do
--     case files of
--       []     -> pure Map.empty
--       x : xs -> do
--         t  <- Stache.compileMustacheFile x
--         ts <- forP xs Stache.compileMustacheFile
--         pure . Stache.templateCache $ sconcat (t :| ts)

--   pure $ \(Stache.PName -> pname) -> do
--     cache <- getCache
-- #else
--   cache <- do
--     case files of
--       []     -> pure Map.empty
--       x : xs -> do
--         t  <- Stache.compileMustacheFile x
--         ts <- forM xs Stache.compileMustacheFile
--         pure . Stache.templateCache $ sconcat (t :| ts)

--   pure $ \(Stache.PName -> pname) -> do
-- #endif
--     let
--       deps = cache Map.!? pname
--         >>= getPartials cache (Set.singleton pname)
--     forM deps \deps -> do
--       needTemplates (Text.unpack . Stache.unPName <$> Set.toList deps)
--       pure $ Template pname (Map.restrictKeys cache deps)
--   where
--     getPartials cache !r (Partial n _ : xs) = do
--       r' <- cache Map.!? n
--         >>= getPartials cache (Set.insert n r)
--       getPartials cache r' xs
--     getPartials cache !r (_ : xs) = getPartials cache r xs
--     getPartials _ !r [] = pure r

--     needTemplates = need . fmap (\x -> dir </> x <.> ".mustache")

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
