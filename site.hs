module Main
    ( main
    ) where

import Data.Foldable
import Hakyll        hiding (defaultContext, pandocCompiler)
import Utils

main :: IO ()
main = hakyll $ do
    match "templates/*" $ compile templateBodyCompiler

    -- TODO: concat css
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["pages/404.md"]) $ do
        route   $ basename `composeRoutes` setExtension "html"
        compile $ pandocCC
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "pages/index.md" $ do
        route   $ basename `composeRoutes` setExtension "html"
        let indexCtx = listField "posts" postCtx postList
              <> defaultContext

        compile $ pandocCCPre (applyAsTemplate indexCtx)
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCC
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["posts/index.html"] $ version "generated" $ do
        route idRoute
        compile $ do
            let postListCtx = fold
                  [ listField "posts" postCtx postList
                  , constField "title" "Posts"
                  , defaultContext
                  ]

            makeItem "$partial(\"templates/post-list.html\")$"
                >>= loadAndApplyTemplate "templates/default.html" postListCtx
                >>= relativizeUrls

    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- recentFirst =<<
              loadAllSnapshots postsPattern "content"
            renderAtom atomFeed feedCtx posts
  where
    postsPattern :: Pattern
    postsPattern = "posts/*" .&&. hasNoVersion

    -- tagsList = buildTags

    postList :: Compiler [Item String]
    postList = recentFirst =<< loadAll postsPattern
