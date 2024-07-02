{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      :  Text.MMark.Util
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal utilities.
module Text.MMark.Util
  ( asPlainText,
    lucidAttributes
  )
where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict    as Map
import           Data.Monoid        (Last (..))
import           Data.Text          (Text)
import           Lucid
import           Text.MMark.Type

-- | Convert a non-empty collection of 'Inline's into their plain text
-- representation. This is used e.g. to render image descriptions.
asPlainText :: NonEmpty Inline -> Text
asPlainText = foldMap $ \case
  Plain txt -> txt
  LineBreak -> "\n"
  Emphasis xs -> asPlainText xs
  Strong xs -> asPlainText xs
  Strikeout xs -> asPlainText xs
  Subscript xs -> asPlainText xs
  Superscript xs -> asPlainText xs
  CodeSpan txt -> txt
  Link xs _ _ -> asPlainText xs
  Image xs _ _ -> asPlainText xs
  RawInline txt -> txt
  Math _ txt -> txt
  Span _ xs -> asPlainText xs

-- | Convert 'Attributes' to lists of Lucid 'Attribute's
lucidAttributes :: Attributes -> [Attribute]
lucidAttributes Attributes{..} =
  Map.foldrWithKey (\k v acc -> term k v : acc) attrs pairs
  where
    attrs = case getLast identifier of
      Nothing -> [classes_ classes]
      Just x  -> [id_ x, classes_ classes]
