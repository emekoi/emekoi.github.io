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

import           Text.MMark.Type

-- | Apply block transformation in the @'Endo' 'Bni'@ form to a block 'Bni'.
applyBlockTrans :: Monad m => EndoM m Bni -> Bni -> m Bni
applyBlockTrans t@(EndoM f) = \case
  Blockquote xs    -> s xs >>= f . Blockquote
  OrderedList w xs -> traverse s xs >>= f . OrderedList w
  UnorderedList xs -> traverse s xs >>= f . UnorderedList
  Div a xs         -> s xs >>= f . Div a
  other -> f other
  where
    s = traverse (applyBlockTrans t)

-- | Apply inline transformation in the @'Endo' 'Inline'@ form to an
-- 'Inline'.
applyInlineTrans :: Monad m => EndoM m Inline -> Inline -> m Inline
applyInlineTrans t@(EndoM f) = \case
  Emphasis xs     -> s xs >>= f . Emphasis
  Strong xs       -> s xs >>= f . Strong
  Strikeout xs    -> s xs >>= f . Strikeout
  Subscript xs    -> s xs >>= f . Subscript
  Superscript xs  -> s xs >>= f . Superscript
  Link xs uri mt  -> s xs >>= f . (\ys -> Link ys uri mt)
  Image xs uri mt -> s xs >>= f . (\ys -> Image ys uri mt)
  Span attrs xs   -> s xs >>= f . Span attrs
  other           -> f other
  where
    s = traverse (applyInlineTrans t)
