{-# LANGUAGE QuasiQuotes     #-}

module Blog.MMark
    ( Document (..)
    , Blog.MMark.parse
    ) where

import           Data.Aeson.Types      (Value (..))
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import Lucid
import qualified Text.Megaparsec.Error as Mega
import Text.MMark as MMark
import Text.MMark.Extension as MMark
import Text.MMark.Type hiding (Render)
import Text.MMark.Trans
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Text.URI as URI
import Blog.Slug
import Control.Monad
import Data.Char (isSpace)
import Data.Function (fix)
import Control.Arrow

data Document = Doc
  { meta :: Value
  , body :: Html ()
  }
  deriving (Show)

parse :: Text -> Text -> Either String Document
parse (Text.unpack -> input) source = do
  case MMark.parse input source of
    Left errs -> Left $ Mega.errorBundlePretty errs
    Right r -> pure $ Doc (fromMaybe Null $ projectYaml r) (MMark.render $ useExtensions extensions r)
  where
    -- extensions = [blockTrans addSectionLinks]
    extensions = [addSectionLinks]

addSectionLinks :: Monad m => Extension m
addSectionLinks = blockTrans $ pure . \case
  Heading1 x -> Heading1 $ Link x (fragment x) Nothing :| []
  Heading2 x -> Heading2 $ Link x (fragment x) Nothing :| []
  Heading3 x -> Heading3 $ Link x (fragment x) Nothing :| []
  Heading4 x -> Heading4 $ Link x (fragment x) Nothing :| []
  Heading5 x -> Heading5 $ Link x (fragment x) Nothing :| []
  Heading6 x -> Heading6 $ Link x (fragment x) Nothing :| []
  block -> block
  where
    fragment (asPlainText -> x) = URI.URI
        { uriScheme = Nothing,
          uriAuthority = Left False,
          uriPath = Nothing,
          uriQuery = [],
          uriFragment = URI.mkFragment (titleSlug x)
        }

-- mkHeader ::([Attribute] -> Html () -> Html ()) -> Ois -> Html () -> Html ()
-- mkHeader f i h = f [id_ uri] (a_ [href_ $ "#" <> uri] h) >> newline
--   where uri = titleSlug (asPlainText $ getOis i)

-- addHeaderIds :: Extension
-- addHeaderIds = blockRender $ \old block ->
--   case block of
--     h@(Heading1 (i,html)) -> mkHeader h1_ i html
--     h@(Heading2 (i,_)) -> withId (old h) i
--     h@(Heading3 (i,_)) -> withId (old h) i
--     h@(Heading4 (i,_)) -> withId (old h) i
--     h@(Heading5 (i,_)) -> withId (old h) i
--     h@(Heading6 (i,_)) -> withId (old h) i
--     other -> old other
--   where
--     withId h i = with h [id_ (headerId (getOis i))]

-- imgLazyLoad :: Pandoc -> Pandoc
-- imgLazyLoad = walk \case
--   Image (id, cs, kv) alt target ->
--     Image (id, cs, ("loading", "lazy") : kv) alt target
--   inline -> inline
