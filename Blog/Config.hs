{-# LANGUAGE TypeFamilies #-}

module Blog.Config
    ( module Blog.Config
    ) where

import           Data.Aeson                ((.=))
import           Data.Aeson.Types          (Object)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Development.Shake
import           Development.Shake.Classes

newtype GitHash = GitHash ()
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult GitHash = Text

gitHash :: Rules (GitHash -> Action Text)
gitHash =
  addOracle $ \(GitHash _) -> Text.strip . Text.decodeUtf8 . fromStdout <$>
    cmd ("git rev-parse --short master" :: String)

author, email, github :: Text
author = "Emeka Nkurumeh"
email = "e.nk@caltech.edu"
github = "https://github.com/emekoi"

siteLang, siteTitle, siteSource, siteURL :: Text
siteLang = "en"
siteTitle = author <> "'s Blog"
siteSource = "https://github.com/emekoi/emekoi.github.io"
siteURL = "https://emekoi.github.io"

defaultCSLFile :: String
defaultCSLFile = "bib/ieee.csl"

baseMetadata :: Action Object
baseMetadata = do
  hash <- askOracle (GitHash ())
  pure
    [ "author"      .= author
    , "lang"        .= siteLang
    , "github"      .= github
    , "site-source" .= siteSource
    , "site-title"  .= siteTitle
    , "site-url"    .= siteURL
    , "email"       .= email
    , "git-hash"    .= hash
    ]
