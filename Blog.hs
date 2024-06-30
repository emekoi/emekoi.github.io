{-# OPTIONS_GHC -Wall -Wextra -Wno-name-shadowing #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Main where

import           Data.Time.Clock
-- import Data.Time.Format

import           Data.Map.Strict (Map)
import           Data.Text       (Text)
import           Ninja

newtype Tag = Tag Text

data Post = Info
  { title     :: Text
  , slug      :: Text
  , published :: UTCTime
  , updated   :: Maybe UTCTime
  , tags      :: [Tag]
  , other     :: Map Text Text
  }

-- data Metadata = Meta
--   { posts  :: [Post]
--   , drafts :: [Post]
--   }

-- newtype Task c k v = Task
--   { runTask :: forall f. c f => (k -> f v) -> f v
--   }

main :: IO ()
main = writeNinja do
  generator "Blog.hs"
