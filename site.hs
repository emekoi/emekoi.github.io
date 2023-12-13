module Main
    ( main
    ) where

import Data.Foldable
import Hakyll                        hiding (defaultContext, pandocCompiler)
import Hakyll                        qualified as H
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
        compile $ H.pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["posts/index.html"] $ version "generated" $ do
        route idRoute
        compile $ do
            let archiveCtx = fold
                  [ listField "posts" postCtx postList
                  , constField "title" "Posts"
                  , defaultContext
                  ]

            makeItem "$partial(\"templates/post-list.html\")$"
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

  where
    postList :: Compiler [Item String]
    postList = recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
