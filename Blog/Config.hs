module Blog.Config
    ( module Blog.Config
    ) where

import Blog.Type
import Data.List qualified as List
import Data.Ord  qualified as Ord
import Data.Text (Text)

author      :: Text
description :: Text
email       :: Text
git         :: Text
lang        :: Text
source      :: Text
title       :: Text
url         :: Text

author      = "Emeka Nkurumeh"
description = author <> "'s Blog"
email       = "e.nk@caltech.edu"
git         = "https://github.com/emekoi"
lang        = "en"
source      = "https://github.com/emekoi/emekoi.github.io"
title       = author <> "'s Blog"
url         = "https://emekoi.github.io"

siteOutput :: FilePath
siteOutput = "_site"

siteBuild :: Text -> [Post] -> Site
siteBuild hash posts = Site
  { author
  , description
  , email
  , git
  , hash
  , lang
  , posts = List.sortOn (Ord.Down . (.published)) posts
  , source
  , tags  = foldMap (.tags) posts
  , title
  , url
  }
