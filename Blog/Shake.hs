{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies   #-}

module Blog.Shake
  ( module Blog.Shake
  ) where

import           Control.Monad
import           Data.Aeson
import qualified Data.Char                 as Char
import qualified Data.List                 as List
import           Data.List.NonEmpty        (NonEmpty (..))
import           Data.Set                  (Set)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Data.Text.Lazy            as TextL
import           Data.Time.Clock
import           Development.Shake
import           Development.Shake.Classes
import           GHC.Generics
import           Lucid
import qualified Text.URI                  as URI

aesonOptions :: Options
aesonOptions = defaultOptions
  { fieldLabelModifier =
    List.intercalate "-"
      . map (map Char.toLower)
      . List.groupBy (const Char.isLower)
  }

newtype GitHash = GitHash String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult GitHash = Text

gitHashOracle :: Rules ()
gitHashOracle =
  void . addOracle $ \(GitHash branch) -> Text.strip . Text.decodeUtf8 . fromStdout <$>
    cmd @(String -> [String] -> Action _) "git" ["rev-parse", "--short", branch]

newtype Tag = Tag Text
  deriving (Eq, Ord, ToJSON, FromJSON)

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

data Post = Post
  { body      :: TextL.Text
  , direction :: Maybe Text
  , hideTitle :: Bool
  , published :: Maybe UTCTime
  , subtitle  :: Maybe Text
  , tags      :: Set Tag
  , title     :: Text
  , updated   :: Maybe UTCTime
  , url       :: Maybe Text
  }
  deriving (Generic)

instance ToJSON Post where
  toJSON     = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

instance FromJSON Post where
  parseJSON = genericParseJSON aesonOptions

data Site = Site
  { author :: Text
  , email  :: Text
  , github :: Text
  , hash   :: Text
  , lang   :: Text
  , source :: Text
  , title  :: Text
  , url    :: Text
  }
  deriving (Generic)

instance ToJSON Site where
  toJSON     = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

instance FromJSON Site where
  parseJSON = genericParseJSON aesonOptions

defaultSite :: Action Site
defaultSite = do
  hash <- askOracle (GitHash "master")
  pure Site
    { author
    , email = "e.nk@caltech.edu"
    , github = "https://github.com/emekoi"
    , hash
    , lang = "en"
    , source = "https://github.com/emekoi/emekoi.github.io"
    , title = author <> "'s Blog"
    , url = "https://emekoi.github.io"
    }
  where author = "Emeka Nkurumeh"

data Posts = Posts
  { posts :: [Post]
  , tags  :: Set Tag
  }

posts :: [Post] -> Posts
posts x = Posts (List.sortOn (.published) x) (foldMap (.tags) x)
