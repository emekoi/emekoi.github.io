{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.MMark.ExtensionSpec
    ( spec
    ) where

import Data.Functor.Identity
import Data.List.NonEmpty    (NonEmpty (..))
import Data.Text             (Text)
import Data.Text             qualified as T
import Lucid                 qualified as L
import Test.Hspec
import Test.QuickCheck
import Text.MMark            qualified as MMark
import Text.MMark.Extension  (Block (..), Inline (..))
import Text.MMark.Extension  qualified as Ext
import Text.MMark.TestUtils

spec :: Spec
spec = parallel $ do
  describe "blockTrans" $ do
    it "works" $ do
      doc <- mkDoc "# My heading"
      toText (MMark.useExtension h1_to_h2 doc)
        `shouldBe` "<h2>My heading</h2>\n"
    it "extensions can affect nested block structures" $ do
      doc <- mkDoc "* # My heading"
      toText (MMark.useExtension h1_to_h2 doc)
        `shouldBe` "<ul>\n<li>\n<h2>My heading</h2>\n</li>\n</ul>\n"
  describe "blockRender" $ do
    it "works" $ do
      doc <- mkDoc "# My heading"
      toText (MMark.useExtension add_h1_content doc)
        `shouldBe` "<h1 data-content=\"My heading\">My heading</h1>\n"
    it "extensions can affect nested block structures" $ do
      doc <- mkDoc "* # Something"
      toText (MMark.useExtension add_h1_content doc)
        `shouldBe` "<ul>\n<li>\n<h1 data-content=\"Something\">Something</h1>\n</li>\n</ul>\n"
  describe "inlineTrans" $ do
    it "works" $ do
      doc <- mkDoc "# My *heading*"
      toText (MMark.useExtension em_to_strong doc)
        `shouldBe` "<h1>My <strong>heading</strong></h1>\n"
    it "extensions can affect nested inline structures" $ do
      doc <- mkDoc "# My ~*heading*~"
      toText (MMark.useExtension em_to_strong doc)
        `shouldBe` "<h1>My <sub><strong>heading</strong></sub></h1>\n"
  describe "inlineRender" $ do
    it "works" $ do
      doc <- mkDoc "# My *heading*"
      toText (MMark.useExtension (add_em_class "foo") doc)
        `shouldBe` "<h1>heading</em></h1>\n"
    it "extensions can affect nested inline structures" $ do
      doc <- mkDoc "[*heading*](/url)"
      toText (MMark.useExtension (add_em_class "foo") doc)
        `shouldBe` "<p><a href=\"/url\"><em class=\"foo\">heading</em></a></p>\n"
  describe "asPlainText" $ do
    let f x = Ext.asPlainText (x :| [])
    context "with Plain" $
      it "works" $
        property $ \txt ->
          f (Plain txt) `shouldBe` txt
    context "with LineBreak" $
      it "works" $
        f LineBreak `shouldBe` "\n"
    context "with Emphasis" $
      it "works" $
        property $ \txt ->
          f (Emphasis $ Plain txt :| []) `shouldBe` txt
    context "with Strong" $
      it "works" $
        property $ \txt ->
          f (Strong $ Plain txt :| []) `shouldBe` txt
    context "with Strikeout" $
      it "works" $
        property $ \txt ->
          f (Strikeout $ Plain txt :| []) `shouldBe` txt
    context "with Subscript" $
      it "works" $
        property $ \txt ->
          f (Subscript $ Plain txt :| []) `shouldBe` txt
    context "with Superscript" $
      it "works" $
        property $ \txt ->
          f (Superscript $ Plain txt :| []) `shouldBe` txt
    context "with CodeSpan" $
      it "works" $
        property $ \txt ->
          f (CodeSpan txt) `shouldBe` txt
    context "with Link" $
      it "works" $
        property $ \txt uri ->
          f (Link (Plain txt :| []) uri Nothing) `shouldBe` txt
    context "with Image" $
      it "works" $
        property $ \txt uri ->
          f (Image (Plain txt :| []) uri Nothing) `shouldBe` txt

----------------------------------------------------------------------------
-- Arbitrary instances

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

----------------------------------------------------------------------------
-- Testing extensions

-- | Convert H1 headings into H2 headings.
h1_to_h2 :: MMark.Extension Identity
h1_to_h2 = Ext.blockTrans $ \case
  Heading1 inner -> pure $ Heading2 inner
  other -> pure other

-- | Add a data attribute calculated based on plain text contents of the
-- level 1 heading to test the 'Ext.getOis' thing and 'Ext.blockRender' in
-- general.
add_h1_content :: MMark.Extension Identity
add_h1_content = Ext.blockRender $ \old block ->
  case block of
    Heading1 inner ->
      L.with
        (old (Heading1 inner))
        [L.data_ "content" (Ext.asPlainText . Ext.getOis . fst $ inner)]
    other -> old other

-- | Convert all 'Emphasis' to 'Strong'.
em_to_strong :: MMark.Extension Identity
em_to_strong = Ext.inlineTrans $ \case
  Emphasis inner -> pure $ Strong inner
  other -> pure other

-- | Add given class to all 'Emphasis' things.
add_em_class :: Text -> MMark.Extension Identity
add_em_class given = Ext.inlineRender $ \old inline ->
  case inline of
    Emphasis inner -> L.with (old (Emphasis inner)) [L.class_ given]
    other          -> old other
