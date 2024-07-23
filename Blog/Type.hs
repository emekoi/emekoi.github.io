{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Blog.Type
    ( DisplayTime (..)
    , FileError (..)
    , Page (..)
    , Post (..)
    , Site (..)
    , Tag (..)
    , Time (..)
    , fileError
    , linkifyTags
    , pattern Time
    , tagLink
    ) where

import Blog.Util                  (titleSlug)
import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson                 (FromJSON (..), ToJSON (..))
import Data.Aeson                 qualified as Aeson
import Data.Aeson.Key             qualified as Aeson
import Data.Char                  qualified as Char
import Data.List                  qualified as List
import Data.List.NonEmpty         (NonEmpty (..))
import Data.Set                   (Set)
import Data.Set                   qualified as Set
import Data.Text                  (Text)
import Data.Text                  qualified as Text
import Data.Text.Lazy             qualified as TextL
import Data.Time
import Data.Time.Format.ISO8601
import Development.Shake.Classes
import GHC.Generics
import Language.Haskell.TH        qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Lucid
import Text.URI                   qualified as URI

fieldMod :: String -> String
fieldMod =
  List.intercalate "-"
    . map (map Char.toLower)
    . List.groupBy (\_ y -> Char.isLower y || not (Char.isAlpha y))

aesonOptions :: Aeson.Options
aesonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = fieldMod
  }

newtype Tag
  = Tag Text
  deriving (Binary, Eq, FromJSON, Hashable, NFData, Ord, Show, ToJSON, Typeable)

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

linkifyTags :: Set Tag -> Set Tag
linkifyTags = Set.map (Tag
  . TextL.toStrict
  . renderText
  . \(Tag t) -> a_ [href_ (URI.render $ tagLink (Tag t))] ("#" <> toHtmlRaw t))

newtype Time
  = MkTime UTCTime
  deriving (Eq, Generic, Hashable, NFData, Ord, Show, Typeable)

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

data DisplayTime = DisplayTime
  { time    :: Maybe Time
  , fmt     :: Maybe String
  , iso8601 :: Maybe String
  }
  deriving (Generic, Show, Typeable)

instance Eq DisplayTime where
  a == b = a.time == b.time

instance Ord DisplayTime where
  a `compare` b = a.time `compare` b.time

instance Semigroup DisplayTime where
  a <> b = DisplayTime
    (a.time <|> b.time)
    (a.fmt <|> b.fmt)
    (a.iso8601 <|> b.iso8601)

instance Hashable DisplayTime
instance Binary DisplayTime
instance NFData DisplayTime

instance ToJSON DisplayTime where
  toJSON     = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

instance FromJSON DisplayTime where
  parseJSON = Aeson.genericParseJSON aesonOptions

data Post = Post
  { body      :: TextL.Text
  , direction :: Maybe Text
  , hideTitle :: Bool
  , published :: DisplayTime
  , subtitle  :: Maybe Text
  , tags      :: Set Tag
  , title     :: Text
  , updated   :: DisplayTime
  , slug      :: Text
  }
  deriving (Eq, Generic, Typeable)

instance Show Post where
  show Post{..} = "Post{" ++ show title ++ "}"

instance Hashable Post
instance Binary Post
instance NFData Post

instance ToJSON Post where
  toJSON     = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

instance FromJSON Post where
  parseJSON = Aeson.withObject "Post" \o -> do
    direction <- o .:? "direction"
    hideTitle <- o .:? "hideTitle" .!= False
    published <- o .:? "published"
    subtitle  <- o .:? "subtitle"
    tags      <- o .:? "tags" .!= mempty
    title     <- o .: "title"
    -- updated   <- fmap (<|> published) (o .:? "updated")
    updated   <- o .:? "updated"
    slug      <- o .:? "slug" .!= titleSlug title
    pure Post
      { body      = mempty
      , published = DisplayTime
        { time    = published
        , fmt     = fmtTime <$> published
        , iso8601 = isoTime <$> published
        }
      , updated   = DisplayTime
        { time    = updated
        , fmt     = fmtTime <$> updated
        , iso8601 = isoTime <$> updated
        }
      , ..
      }
    where
      fmtTime (MkTime t) = formatTime defaultTimeLocale "%e %B %Y" t
      isoTime (MkTime t) = iso8601Show t
      (.:) x y = x Aeson..: Aeson.fromString (fieldMod y)
      (.:?) x y = x Aeson..:? Aeson.fromString (fieldMod y)
      (.!=) = (Aeson..!=)

data Site = Site
  { author      :: Text
  , description :: Text
  , email       :: Text
  , git         :: Text
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

data Page = Page
  { meta :: Aeson.Object
  , body :: TextL.Text
  }
  deriving (Eq, Generic, Show, Typeable)

instance Hashable Page
instance NFData Page

instance ToJSON Page where
  toJSON     = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

instance FromJSON Page where
  parseJSON = Aeson.genericParseJSON aesonOptions

instance Binary Page where
  put = put . Aeson.encode
  get = do
    Just x <- Aeson.decode <$> get
    pure x

instance TH.Lift Page where
  liftTyped p = TH.unsafeCodeCoerce (TH.lift p)
  lift (Page m t) =
    liftA2 (\m t -> TH.ConE 'Page `TH.AppE` m `TH.AppE` t) (TH.lift m) (TH.lift t)

data FileError = FileError
  { path :: Maybe FilePath
  , msg  :: String
  }
  deriving (Generic, Show, Typeable)

instance Exception FileError where
  displayException p = p.msg

fileError :: MonadIO m => Maybe FilePath -> String -> m a
fileError path msg = liftIO . throwIO $ FileError {..}
