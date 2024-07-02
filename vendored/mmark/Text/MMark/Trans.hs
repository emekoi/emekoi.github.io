{-# LANGUAGE LambdaCase #-}

-- |
-- Module      :  Text.MMark.Trans
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- MMark block\/inline transformation helpers.
module Text.MMark.Trans
  ( applyBlockTrans,
    applyInlineTrans,
  )
where

import           Control.Arrow
import           Text.MMark.Type

-- | Apply block transformation in the @'Endo' 'Bni'@ form to a block 'Bni'.
applyBlockTrans :: Monad m => Kleisli m Bni Bni -> Bni -> m Bni
applyBlockTrans trans@(Kleisli f) = \case
  Blockquote xs -> f =<< (Blockquote <$> s xs)
  OrderedList w xs -> f =<< (OrderedList w <$> (traverse s xs))
  UnorderedList xs -> f =<< (UnorderedList <$> (traverse s xs))
  other -> f other
  where
    s = traverse (applyBlockTrans trans)

-- | Apply inline transformation in the @'Endo' 'Inline'@ form to an
-- 'Inline'.
applyInlineTrans :: Monad m => Kleisli m Inline Inline -> Inline -> m Inline
applyInlineTrans trans@(Kleisli f) = \case
  Emphasis xs -> f =<< (Emphasis <$> (s xs))
  Strong xs -> f =<< (Strong <$> (s xs))
  Strikeout xs -> f =<< (Strikeout <$> (s xs))
  Subscript xs -> f =<< (Subscript <$> (s xs))
  Superscript xs -> f =<< (Superscript <$> (s xs))
  Link xs uri mt -> f =<< (Link <$> (s xs) <*> pure uri <*> pure mt)
  Image xs uri mt -> f =<< (Image <$> (s xs) <*> pure uri <*> pure mt)
  other -> f other
  where
    s = traverse (applyInlineTrans trans)
