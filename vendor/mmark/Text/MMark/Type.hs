{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

-- |
-- Module      :  Text.MMark.Type
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal type definitions. Some of these are re-exported in the public
-- modules.
module Text.MMark.Type
    ( Attributes (..)
    , Block (..)
    , Bni
    , CellAlign (..)
    , Endo (..)
    , EndoM (..)
    , Extension (..)
    , Inline (..)
    , MMark (..)
    , MathType (..)
    , Ois
    , Render
    , getOis
    , mkOisInternal
    ) where

import Control.DeepSeq
import Control.Monad      ((>=>))
import Data.Aeson
import Data.Data          (Data)
import Data.Function      (on)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict    (Map)
import Data.Monoid        (Endo (..), Last (..))
import Data.Text          (Text)
import Data.Typeable      (Typeable)
import GHC.Generics
import Lucid
import Text.URI           (URI (..))

newtype EndoM m a
  = EndoM { appEndoM :: a -> m a }

instance Monad m => Semigroup (EndoM m a) where
  EndoM f <> EndoM g = EndoM (g >=> f)

instance Monad m => Monoid (EndoM m a) where
  mempty = EndoM pure

-- | Representation of complete markdown document. You can't look inside of
-- 'MMark' on purpose. The only way to influence an 'MMark' document you
-- obtain as a result of parsing is via the extension mechanism.
data MMark m = MMark
  { -- | Parsed YAML document at the beginning (optional)
    mmarkYaml      :: Maybe Object
    -- | Actual contents of the document
  , mmarkBlocks    :: [Bni]
    -- | Extension specifying how to process and render the blocks
  , mmarkExtension :: Extension m
  }

instance NFData (MMark m) where
  rnf MMark {..} = rnf mmarkYaml `seq` rnf mmarkBlocks

-- | Dummy instance.
--
-- @since 0.0.5.0
instance Show (MMark m) where
  show = const "MMark {..}"

-- | An extension. You can apply extensions with 'Text.MMark.useExtension'
-- and 'Text.MMark.useExtensions' functions. The "Text.MMark.Extension"
-- module provides tools for writing your own extensions.
--
-- Note that 'Extension' is an instance of 'Semigroup' and 'Monoid', i.e.
-- you can combine several extensions into one. Since the @('<>')@ operator
-- is right-associative and 'mconcat' is a right fold under the hood, the
-- expression
--
-- > l <> r
--
-- means that the extension @r@ will be applied before the extension @l@,
-- similar to how 'Endo' works. This may seem counter-intuitive, but only
-- with this logic we get consistency of ordering with more complex
-- expressions:
--
-- > e2 <> e1 <> e0 == e2 <> (e1 <> e0)
--
-- Here, @e0@ will be applied first, then @e1@, then @e2@. The same applies
-- to expressions involving 'mconcat'—extensions closer to beginning of the
-- list passed to 'mconcat' will be applied later.
data Extension m = Extension
  { -- | Block transformation
    extBlockTrans   :: EndoM m Bni
    -- | Block render
  , extBlockRender  :: Render m (Block (Ois, HtmlT m ()))
    -- | Inline transformation
  , extInlineTrans  :: EndoM m Inline
    -- | Inline render
  , extInlineRender :: Render m Inline
  }

instance Monad m => Semigroup (Extension m) where
  x <> y =
    Extension
      { extBlockTrans = on (<>) extBlockTrans x y,
        extBlockRender = on (<>) extBlockRender x y,
        extInlineTrans = on (<>) extInlineTrans x y,
        extInlineRender = on (<>) extInlineRender x y
      }

instance Monad m => Monoid (Extension m) where
  mempty =
    Extension
      { extBlockTrans = mempty,
        extBlockRender = mempty,
        extInlineTrans = mempty,
        extInlineRender = mempty
      }
  mappend = (<>)

-- | An internal type that captures the extensible rendering process we use.
-- 'Render' has a function inside which transforms a rendering function of
-- the type @a -> Html ()@.
type Render m a = Endo (a -> HtmlT m ())

-- | Attributes associated with a 'Div'.
data Attributes = Attributes
  { identifier :: Last Text
  , classes    :: [Text]
  , pairs      :: Map Text Text
  }
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance Semigroup Attributes where
  Attributes x1 x2 x3 <> Attributes y1 y2 y3 =
    Attributes (x1 <> y1) (x2 <> y2) (x3 <> y3)

instance Monoid Attributes where
  mempty = Attributes mempty mempty mempty

instance NFData Attributes

-- | A shortcut for the frequently used type @'Block' ('NonEmpty'
-- 'Inline')@.
type Bni = Block (NonEmpty Inline)

-- | We can think of a markdown document as a collection of
-- blocks—structural elements like paragraphs, block quotations, lists,
-- headings, thematic breaks, and code blocks. Some blocks (like block
-- quotes and list items) contain other blocks; others (like headings and
-- paragraphs) contain inline content, see 'Inline'.
--
-- We can divide blocks into two types: container blocks, which can contain
-- other blocks, and leaf blocks, which cannot.
data Block a
  -- | Thematic break, leaf block
  = ThematicBreak
  -- | Heading (level 1), leaf block
  | Heading1 a
  -- | Heading (level 2), leaf block
  | Heading2 a
  -- | Heading (level 3), leaf block
  | Heading3 a
  -- | Heading (level 4), leaf block
  | Heading4 a
  -- | Heading (level 5), leaf block
  | Heading5 a
  -- | Heading (level 6), leaf block
  | Heading6 a
  -- | Code block, leaf block with info string and contents
  | CodeBlock Attributes Text
  -- | Naked content, without an enclosing tag
  | Naked a
  -- | Paragraph, leaf block
  | Paragraph a
  -- | Blockquote container block
  | Blockquote [Block a]
  -- | Ordered list ('Word' is the start index), container block
  | OrderedList Word (NonEmpty [Block a])
  -- | Unordered list, container block
  | UnorderedList (NonEmpty [Block a])
  -- | Table, first argument is the alignment options, then we have a
  -- 'NonEmpty' list of rows, where every row is a 'NonEmpty' list of
  -- cells, where every cell is an @a@ thing.
  --
  -- The first row is always the header row, because pipe-tables that we
  -- support cannot lack a header row.
  --
  -- @since 0.0.4.0
  | Table (NonEmpty CellAlign) (NonEmpty (NonEmpty a))
  -- | Divs
  | Div Attributes [Block a]
  -- | Footnote Definition
  | Note Text [Block a]
  deriving
    ( Data
    , Eq
    , Foldable
    , Functor
    , Generic
    , Ord
    , Show
    , Traversable
    , Typeable
    )

instance (NFData a) => NFData (Block a)

-- | Options for cell alignment in tables.
--
-- @since 0.0.4.0
data CellAlign
  -- | No specific alignment specified
  = CellAlignDefault
  -- | Left-alignment
  | CellAlignLeft
  -- | Right-alignment
  | CellAlignRight
  -- | Center-alignment
  | CellAlignCenter
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance NFData CellAlign

data MathType
  = InlineMath
  | DisplayMath
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance NFData MathType

-- | Inline markdown content.
data Inline
  -- | Plain text
  = Plain Text
  -- | Line break (hard)
  | LineBreak
  -- | Emphasis
  | Emphasis (NonEmpty Inline)
  -- | Strong emphasis
  | Strong (NonEmpty Inline)
  -- | Strikeout
  | Strikeout (NonEmpty Inline)
  -- | Subscript
  | Subscript (NonEmpty Inline)
  -- | Superscript
  | Superscript (NonEmpty Inline)
  -- | Code span
  | CodeSpan Text
  -- | Link with text, destination, and optionally title
  | Link (NonEmpty Inline) URI (Maybe Text)
  -- | Image with description, URL, and optionally title
  | Image (NonEmpty Inline) URI (Maybe Text)
  -- | Raw inline content
  | RawInline Text
  -- | Math
  | Math MathType Text
  -- | Spans
  | Span Attributes (NonEmpty Inline)
  -- | Footnote Reference
  | NoteRef Text
  deriving (Data, Eq, Generic, Ord, Show, Typeable)

instance NFData Inline

-- | A wrapper for “original inlines”. Source inlines are wrapped in this
-- during rendering of inline components and then it's available to block
-- render, but only for inspection. Altering of 'Ois' is not possible
-- because the user cannot construct a value of the 'Ois' type, he\/she can
-- only inspect it with 'getOis'.
newtype Ois
  = Ois (NonEmpty Inline)

-- | Make an 'Ois' value. This is an internal constructor that should not be
-- exposed!
mkOisInternal :: NonEmpty Inline -> Ois
mkOisInternal = Ois

-- | Project @'NonEmpty' 'Inline'@ from 'Ois'.
getOis :: Ois -> NonEmpty Inline
getOis (Ois inlines) = inlines
