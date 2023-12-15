module Main
    ( main
    ) where

import Config
import Control.Applicative
import Control.Monad
import Data.Aeson                      qualified as Aeson
import Data.ByteString.Lazy.Char8      qualified as B
import Data.Foldable
import Data.List                       qualified as L (intercalate)
import Data.Map.Strict                 qualified as Map
import Data.Maybe                      (fromJust, fromMaybe)
import Data.Set                        qualified as Set
import Hakyll                          hiding (defaultContext, pandocCompiler)
import Network.HTTP.Types.Status       (status404)
import Network.Wai                     qualified as W
import Slug
import System.FilePath                 ((</>))
import Utils
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
  { previewSettings =
    \path -> let settings = previewSettings defaultConfiguration path in
      settings
        { ss404Handler = Just pageNotFound
        }
  }
  where
    pageNotFound ::  W.Application
    pageNotFound _ respond =
      B.readFile (destinationDirectory defaultConfiguration </> "404.html")
        >>= respond . W.responseLBS status404 []

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = siteTitle
  , feedDescription = author ++ "'s Blog"
  , feedAuthorName  = author
  , feedAuthorEmail = email
  , feedRoot        = siteURL
  }

data PostList
  = Tag String
  | Only Int
  | All

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

jsonTagsCtx :: Context a
jsonTagsCtx = field "tags" \(Item id _) -> do
  meta <- getMetadata id
  pure . B.unpack . Aeson.encode . fromMaybe [] $ lookupStringList "tags" meta

main :: IO ()
main = hakyllWith hakyllConfig do
    match "templates/*" $
      compile templateBodyCompiler

    match ("css/*.css" .&&. complement "css/default.css") $
      compile templateBodyCompiler

    match "css/default.css" do
      route idRoute
      compile $
        getResourceString >>= (fmap (fmap compressCss) . applyAsTemplate mempty)

    match (fromList ["pages/404.md"]) do
      route $ basename `composeRoutes` setExtension "html"
      compile $ pandocCC
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

    match "pages/index.md" do
      route $ basename `composeRoutes` setExtension "html"
      let indexCtx = listField "posts" postCtx (postList (Only 5))
            <> defaultContext

      compile $ pandocCCPre (applyAsTemplate indexCtx)
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

    match ("posts/*.md" .||. "posts/*.lhs") do
      route $ metadataRoute \meta ->
        let
          slug = fromJust $ lookupString "slug" meta
            <|> (titleSlug <$> lookupString "title" meta)
        in  constRoute $ L.intercalate "/" ["posts", slug, "index.html"]

      compile $ pandocCC
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
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
        feedT <- loadBody "templates/atom.xml"
        itemT <- loadBody "templates/atom-item.xml"
        posts <- recentFirst =<< loadAllSnapshots postsPattern "content"
        let feedCtx = postCtx <> bodyField "description"
        renderAtomWithTemplates feedT itemT feedConfig feedCtx posts

    create ["feed.json"] $ version "generated" do
      route idRoute
      compile do
        posts <- recentFirst =<< loadAllSnapshots postsPattern "content"
        feedT <- loadBody "templates/feed.json"
        itemT <- loadBody "templates/feed-item.json"
        let feedCtx = bodyField "description" <> jsonTagsCtx <> postCtx
        renderJsonWithTemplates feedT itemT feedConfig feedCtx posts

  where
    tagPageEntry :: String
    tagPageEntry =
      "<h2 id=\"$tag$\">#$tag$</h2>$partial(\"templates/post-list.html\")$"
