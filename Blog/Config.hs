module Blog.Config
    ( module Blog.Config
    ) where

import Blog.Type
import Data.List qualified as List
import Data.Ord  qualified as Ord

author      :: StrictText
description :: StrictText
email       :: StrictText
git         :: StrictText
lang        :: StrictText
source      :: StrictText
title       :: StrictText
url         :: StrictText

author      = "Emeka Nkurumeh"
description = author <> "'s Blog"
email       = "emnkuru@iu.edu"
git         = "https://github.com/emekoi"
lang        = "en"
source      = "https://github.com/emekoi/emekoi.github.io"
title       = author <> "'s Blog"
url         = "https://emekoi.github.io"

siteOutput :: FilePath
siteOutput = "_site"

siteBuild :: Bool -> StrictText -> [Post] -> Site
siteBuild debug hash posts = Site
  { author
  , debug
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
