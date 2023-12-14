module Main
    ( main
    ) where

import Control.Monad
import Data.Foldable
import Data.List       qualified as L (intercalate)
import Data.Map.Strict qualified as Map
import Data.Maybe      (fromJust)
import Data.Set        qualified as Set
import Hakyll          hiding (defaultContext, pandocCompiler)
import Utils

data PostList
  = Tag String
  | Only Int
  | All

main :: IO ()
main = hakyll do
    match "templates/*" $ compile templateBodyCompiler

    -- TODO: concat css
    match "css/*" do
      route idRoute
      compile compressCssCompiler

    match (fromList ["pages/404.md"]) do
      route $ basename `composeRoutes` setExtension "html"
      compile $ pandocCC
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    match "pages/index.md" do
      route $ basename `composeRoutes` setExtension "html"
      let indexCtx = listField "posts" postCtx (postList (Only 5))
            <> defaultContext

      compile $ pandocCCPre (applyAsTemplate indexCtx)
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

    match ("posts/*.md" .||. "posts/*.lhs") do
      postMetadata <- Map.fromList <$> getAllMetadata postsPattern
      titleSlugs <- preprocess do
        traverse (titleSlug . fromJust . lookupString "title") postMetadata

      route $ customRoute \f ->
        L.intercalate "/" ["posts", titleSlugs Map.! f, "index.html"]

      compile $ pandocCC
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

    create ["posts/index.html"] $ version "generated" do
      route idRoute
      compile do
        let postListCtx = fold
              [ listField "posts" postCtx (postList All)
              , constField "title" "Posts"
              , defaultContext
              ]

        makeItem "$partial(\"templates/post-list.html\")$"
          >>= applyAsTemplate postListCtx
          >>= loadAndApplyTemplate "templates/default.html" postListCtx
          >>= relativizeUrls

    create ["tags.html"] $ version "generated" do
      postTags <-
        fmap (maybe Set.empty Set.fromList . lookupStringList "tags")
        . Map.fromList <$> getAllMetadata postsPattern

      route idRoute
      compile do
        let postListCtx tag = constField "tag" tag
              <> listField "posts" postCtx (postList (Tag tag))
            pageCtx = constField "title" "Tags" <> defaultContext

        uItem <- Item <$> getUnderlying
        page <- foldrM `flip` [] `flip` fold postTags $ \tag acc -> do
          Item _ item <- applyAsTemplate (postListCtx tag) $ uItem tagPageEntry
          pure $ item ++ acc

        loadAndApplyTemplate "templates/default.html" pageCtx (uItem page)
          >>= relativizeUrls

    create ["feed.xml"] $ version "generated" do
      route idRoute
      compile do
        let feedCtx = postCtx <> bodyField "description"
        posts <- recentFirst =<<
          loadAllSnapshots postsPattern "content"
        renderAtom atomFeed feedCtx posts

  where
    tagPageEntry :: String
    tagPageEntry =
      "<h2 id=\"$tag$\">#$tag$</h2>$partial(\"templates/post-list.html\")$"

    postsPattern :: Pattern
    postsPattern = "posts/*.md" .||. "posts/*.lhs"

    postList :: PostList -> Compiler [Item String]
    postList p = do
      posts <- recentFirst =<< loadAll postsPattern
      case p of
        All        -> pure posts
        Only n     -> pure $ take n posts
        Tag tag ->
          filterM (\(Item id _) -> elem tag <$> getTags id) posts
