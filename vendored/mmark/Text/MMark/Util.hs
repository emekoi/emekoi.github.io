{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

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
  )
where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text          (Text)
import           Text.MMark.Type

-- | Convert a non-empty collection of 'Inline's into their plain text
-- representation. This is used e.g. to render image descriptions.
asPlainText :: NonEmpty Inline -> Text
asPlainText = foldMap $ \case
  Plain txt -> txt
  Raw txt -> txt
  LineBreak -> "\n"
  Emphasis xs -> asPlainText xs
  Strong xs -> asPlainText xs
  Strikeout xs -> asPlainText xs
  Subscript xs -> asPlainText xs
  Superscript xs -> asPlainText xs
  CodeSpan txt -> txt
  Link xs _ _ -> asPlainText xs
  Image xs _ _ -> asPlainText xs
