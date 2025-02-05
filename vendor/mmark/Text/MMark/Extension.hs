{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Text.MMark.Extension
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides building blocks for creation of extensions.
--
-- We suggest using a qualified import, like this:
--
-- > import Text.MMark.Extension (Bni, Block (..), Inline (..))
-- > import qualified Text.MMark.Extension as Ext
--
-- === The philosophy of MMark extensions
--
-- The extension system is guided by the following goals:
--
--     1. Make it powerful, so users can write interesting extensions.
--     2. Make it efficient, so every type of transformation is only applied
--        once and the number of traversals of the syntax tree stays
--        constant no matter how many extensions the user chooses to use and
--        how complex they are.
--     3. Make it easy to write extensions that are very focused in what
--        they do and do not interfere with each other in weird and
--        unexpected ways.
--
-- I ruled out allowing users to mess with AST directly pretty quickly
-- because it would be against the points 2 and 3. Instead, there are four
-- kinds of extension-producing functions. They correspond internally to
-- four functions that are applied to the parsed document in turn:
--
--     * 'blockTrans' is applied first, as it's quite general and can change
--       block-level structure of document as well as inline-level
--       structure.
--     * 'inlineTrans' is applied to every inline in the document obtained
--       in the previous step.
--     * 'inlineRender' is applied to every inline; this function produces
--       HTML rendition of the inlines and we also preserve the original
--       inlines so 'blockRender' can look at it (see 'Ois').
--     * 'blockRender' is applied to every block to obtain HTML rendition of
--       the whole document.
--
-- When one combines different extensions, extensions of the same kind get
-- fused together into a single function. This allows for faster processing
-- and constant number of traversals over AST in the end.
--
-- One could note that the current design does not allow prepending or
-- appending new elements to the AST. This is a limitation by design because
-- we try to make the order in which extensions are applied unimportant
-- (it's not always possible, though). Thus, if we want to e.g. insert a
-- table of contents into a document, we need to do so by transforming an
-- already existing element, such as code block with a special info string
-- (this is how the extension works in the @mmark-ext@ package).
--
-- Another limitation by design is that extensions cannot change how the
-- parser works. I find endless syntax-changing (or syntax-augmenting, if
-- you will) extensions (as implemented by Pandoc for example) ugly, because
-- they erode the familiar markdown syntax and turn it into a monstrosity.
-- In MMark we choose a different path of re-purposing existing markdown
-- constructs, adding special meaning to them in certain situations.
--
-- === Room for improvement
--
-- One flaw of the current system is that it does not allow reporting
-- errors, so we have to silently fallback to some default behavior when we
-- can't apply an extension in a meaningful way. Such extension-produced
-- errors obviously should contain their positions in the original markdown
-- input, which would require us storing this information in AST in some
-- way. I'm not sure if the additional complexity (and possible performance
-- trade-offs) is really worth it, so it hasn't been implemented so far.
module Text.MMark.Extension
    ( -- * Extension construction
      Extension
      -- ** Block-level manipulation
    , Block (..)
    , Bni
    , CellAlign (..)
    , Ois
    , blockRender
    , blockTrans
    , getOis
      -- ** Inline-level manipulation
    , Inline (..)
    , inlineRender
    , inlineTrans
      -- * Utils
    , asPlainText
    , fold
    , foldM
    ) where

import Lucid
import Text.MMark.Type
import Text.MMark.Util

-- | Create an extension that performs a transformation on 'Block's of
-- markdown document. Since a block may contain other blocks we choose to
-- perform transformations from the most deeply nested blocks moving
-- upwards. This has the benefit that the result of any transformation is
-- final in the sense that sub-elements of resulting block won't be
-- traversed again.
blockTrans :: Monad m => (Bni -> m Bni) -> Extension m
blockTrans f = mempty {extBlockTrans = EndoM f}

-- | Create an extension that replaces or augments rendering of 'Block's of
-- markdown document. The argument of 'blockRender' will be given the
-- rendering function constructed so far @'Block' ('Ois', 'Html' ()) ->
-- 'Html' ()@ as well as an actual block to render—@'Block' ('Ois', 'Html'
-- ())@. The user can then decide whether to replace\/reuse that function to
-- get the final rendering of the type @'Html' ()@.
--
-- The argument of 'blockRender' can also be thought of as a function that
-- transforms the rendering function constructed so far:
--
-- > (Block (Ois, Html ()) -> Html ()) -> (Block (Ois, Html ()) -> Html ())
--
-- See also: 'Ois' and 'getOis'.
blockRender :: Monad m => ((Block (Ois, HtmlT m ()) -> HtmlT m ()) -> Block (Ois, HtmlT m ()) -> HtmlT m ()) -> Extension m
blockRender f = mempty {extBlockRender = Endo f}

-- | Create an extension that performs a transformation on 'Inline'
-- components in entire markdown document. Similarly to 'blockTrans' the
-- transformation is applied from the most deeply nested elements moving
-- upwards.
inlineTrans :: Monad m => (Inline -> m Inline) -> Extension m
inlineTrans f = mempty {extInlineTrans = EndoM f}

-- | Create an extension that replaces or augments rendering of 'Inline's of
-- markdown document. This works like 'blockRender'.
inlineRender :: Monad m => ((Inline -> HtmlT m ()) -> Inline -> HtmlT m ()) -> Extension m
inlineRender f = mempty {extInlineRender = Endo f}

----------------------------------------------------------------------------
-- Folding

-- | Fold over a 'MMark' document efficiently in one pass.
fold ::
  -- | Document to fold over
  MMark m ->
  -- | Fold step
  (x -> Bni -> x) ->
  -- | Fold initial value
  x ->
  -- | Fold extract/finish step
  (x -> b) ->
  -- | Fold result
  b
fold MMark {..} step begin done = foldr cons done mmarkBlocks begin
  where
    cons a k x = k $! step x a
{-# INLINE fold #-}

-- | Like 'fold', but allows us to fold within a monadic context.
foldM
  :: Monad m =>
  -- | Document to fold over
  MMark n ->
  -- | Fold step
  (x -> Bni -> m x) ->
  -- | Fold initial value
  m x ->
  -- | Fold extract/finish step
  (x -> m b) ->
  -- | Fold result
  m b
foldM MMark {..} step begin done = do
  x0 <- begin
  foldr step' done mmarkBlocks $! x0
  where
    step' a k x = do
      x' <- step x a
      k $! x'
{-# INLINE foldM #-}
