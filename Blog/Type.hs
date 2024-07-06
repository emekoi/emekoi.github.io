{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Blog.Type
  ( Post (..)
  , Site (..)
  , Tag (..)
  , Time (..)
  , Page (..)
  , pattern Time
  , linkifyTags
  , tagLink
  ) where

import           Blog.Util                  (titleSlug)
import           Control.Applicative
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Key             as Aeson
import qualified Data.Char                  as Char
import qualified Data.List                  as List
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as TextL
import           Data.Time
import           Data.Time.Format.ISO8601
import           Development.Shake.Classes
import           GHC.Generics
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH
import           Lucid
import qualified Text.URI                   as URI

fieldMod :: String -> String
fieldMod =
  List.intercalate "-"
    . map (map Char.toLower)
    . List.groupBy (\_ y -> Char.isLower y || not (Char.isAlpha y))

aesonOptions :: Aeson.Options
aesonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = fieldMod
  }

newtype Tag = Tag Text
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Ord, ToJSON, FromJSON)

tagLink :: Tag -> URI.URI
tagLink (Tag t) = URI.URI
  { uriScheme = Nothing
  , uriAuthority = Left True
  , uriPath = do
      tags <- URI.mkPathPiece "tags"
      pure (False, tags :| [])
  , uriQuery = []
  , uriFragment = URI.mkFragment t
  }

newtype Time = MkTime UTCTime
  deriving (Generic, Show, Typeable, Eq, Hashable, NFData, Ord)

pattern Time :: Day -> DiffTime -> Time
pattern Time day diff = MkTime (UTCTime day diff)

instance Binary Time where
  put (MkTime t) = put (iso8601Show t)
  get = MkTime <$> (get >>= iso8601ParseM)

instance ToJSON Time where
  toJSON (Time day 0) = toJSON day
  toJSON (MkTime t)   = toJSON t

  toEncoding (Time day 0) = toEncoding day
  toEncoding (MkTime t)   = toEncoding t

instance FromJSON Time where
  parseJSON = Aeson.withText "Time" \(Text.unpack -> o) -> do
    MkTime <$> (iso8601ParseM o <|> (flip UTCTime 0 <$>iso8601ParseM o))

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
  , slug            :: Text
  }
  deriving (Generic, Show, Typeable, Eq)

instance Hashable Post where

instance Binary Post where

instance NFData Post where

instance ToJSON Post where
  toJSON     = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

instance FromJSON Post where
  parseJSON = Aeson.withObject "Post" \o -> do
    direction <- o .:? "direction"
    hideTitle <- o .:? "hideTitle" .!= False
    published <- o .:? "published"
    subtitle  <- o .:? "subtitle"
    tags      <- o .:? "tags" .!= []
    title     <- o .: "title"
    -- updated   <- fmap (<|> published) (o .:? "updated")
    updated   <- o .:? "updated"
    slug      <- o .:? "slug" .!= titleSlug title
    pure Post
      { body            = mempty
      , publishedIs8601 = Nothing
      , updatedIso8601  = Nothing
      , ..
      }

    where
      (.:) x y = x Aeson..: Aeson.fromString (fieldMod y)
      (.:?) x y = x Aeson..:? Aeson.fromString (fieldMod y)
      (.!=) = (Aeson..!=)

data Site = Site
  { author      :: Text
  , description :: Text
  , email       :: Text
  , github      :: Text
  , hash        :: Text
  , lang        :: Text
  , posts       :: [Post]
  , source      :: Text
  , tags        :: Set Tag
  , title       :: Text
  , url         :: Text
  }
  deriving (Generic)

instance ToJSON Site where
  toJSON     = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

instance FromJSON Site where
  parseJSON = Aeson.genericParseJSON aesonOptions

linkifyTags :: Set Tag -> Set Tag
linkifyTags = Set.map (Tag
  . TextL.toStrict
  . renderText
  . \(Tag t) -> a_ [href_ (URI.render $ tagLink (Tag t))] ("#" <> toHtmlRaw t))


data Page = Page
  { meta :: Aeson.Object
  , body :: TextL.Text
  }
  deriving (Show)

instance TH.Lift Page where
  liftTyped p = TH.unsafeCodeCoerce (TH.lift p)
  lift (Page m t) =
    liftA2 (\m t -> TH.ConE 'Page `TH.AppE` m `TH.AppE` t) (TH.lift m) (TH.lift t)
