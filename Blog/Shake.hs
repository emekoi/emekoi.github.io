{-# LANGUAGE DeriveGeneric #-}

module Blog.Shake
  ( Post (..)
  , Site (..)
  , Tag (..)
  , Time (..)
  , emptyPost
  , prettfyTags
  , tagLink
  ) where

import           Data.Aeson                (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                as Aeson
import qualified Data.Char                 as Char
import qualified Data.List                 as List
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as TextL
import           Data.Time
import           Data.Time.Format.ISO8601
import           Development.Shake.Classes
import           GHC.Generics
import           Lucid
import qualified Text.URI                  as URI

aesonOptions :: Aeson.Options
aesonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier =
    List.intercalate "-"
      . map (map Char.toLower)
      . List.groupBy (\_ y -> Char.isLower y || not (Char.isAlpha y))
  }

newtype Tag = Tag Text
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Ord, ToJSON, FromJSON)

tagLink :: Tag ->  URI.URI
tagLink (Tag t) = URI.URI
  { uriScheme = Nothing
  , uriAuthority = Left False
  , uriPath = do
      p <- URI.mkPathPiece t
      pure (False, p :| [])
  , uriQuery = []
  , uriFragment = Nothing
  }

instance ToHtml Tag where
  toHtml t@(Tag x) = li_ $ a_ [href_ (URI.render $ tagLink t)] ("#" <> toHtml x)
  toHtmlRaw t@(Tag x) = li_ $ a_ [href_ (URI.render $ tagLink t)] ("#" <> toHtmlRaw x)

newtype Time = Time Day
  deriving (Generic, Show, Typeable, Eq, Hashable, NFData, Ord, ToJSON, FromJSON)

instance Binary Time where
  put (Time t) = put (iso8601Show t)
  get = Time <$> (get >>= iso8601ParseM)

data Post = Post
  { body            :: TextL.Text
  , direction       :: Maybe Text
  , hideTitle       :: Bool
  , published       :: Maybe Time
  , publishedIs8601 :: Maybe Time
  , subtitle        :: Maybe Text
  , tags            :: Set Tag
  , title           :: Text
  , updated         :: Maybe Time
  , updatedIso8601  :: Maybe Time
  , slug            :: Maybe Text
  }
  deriving (Generic, Show, Typeable, Eq)

emptyPost :: Post
emptyPost = Post
  { body            = mempty
  , direction       = Nothing
  , hideTitle       = False
  , published       = Nothing
  , publishedIs8601 = Nothing
  , subtitle        = Nothing
  , tags            = mempty
  , title           = mempty
  , updated         = Nothing
  , updatedIso8601  = Nothing
  , slug            = Nothing
  }

instance Hashable Post where

instance Binary Post where

instance NFData Post where

instance ToJSON Post where
  toJSON     = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

instance FromJSON Post where
  parseJSON = Aeson.genericParseJSON aesonOptions

data Site = Site
  { author :: Text
  , email  :: Text
  , github :: Text
  , hash   :: Text
  , lang   :: Text
  , posts  :: [Post]
  , source :: Text
  , tags   :: Set Tag
  , title  :: Text
  , url    :: Text
  }
  deriving (Generic)

instance ToJSON Site where
  toJSON     = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

instance FromJSON Site where
  parseJSON = Aeson.genericParseJSON aesonOptions

prettfyTags :: Set Tag -> Set Tag
-- prettfyTags Site{..} = Site{ tags = Set.map (\t -> Tag (TextL.toStrict $ renderText t)) tags, ..}
prettfyTags = Set.map (Tag . TextL.toStrict . renderText . toHtml)
