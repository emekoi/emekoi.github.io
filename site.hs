{-# LANGUAGE QuasiQuotes #-}

module Main
    ( main
    ) where

import Config
import Control.Applicative
import Control.Monad
import Data.Aeson                 qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as B
import Data.Foldable
import Data.List                  qualified as L (intercalate)
import Data.Map.Strict            qualified as Map
import Data.Maybe                 (fromJust, fromMaybe)
import Data.Set                   qualified as Set
import Data.Text                  qualified as T
import Hakyll                     hiding (defaultContext, pandocCompiler)
import Network.HTTP.Types.Status  (status404)
import Network.Wai                qualified as W
import Slug
import System.Directory           (doesFileExist)
import System.FilePath            (takeExtension, (<.>), (</>))
import System.Process
import Text.RawString.QQ
import Utils
import WaiAppStatic.Types

-- https://github.com/LightAndLight/lightandlight.github.io/blob/a29bac1b084b86abe43e28c4062ca963d0647b98/site.hs#L31-L55
hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
  { previewSettings =
    \path -> let settings = previewSettings defaultConfiguration path in
      settings
        { ss404Handler = Just pageNotFound
        , ssLookupFile = \pieces ->
          case splitAt (length pieces - 1) pieces of
            (prefix, [piece])
              | let fileName = fromPiece piece
              , takeExtension (T.unpack fileName) == "" ->
                  settings.ssLookupFile $ prefix <> [unsafeToPiece $ fileName <> ".html"]
            _ -> settings.ssLookupFile pieces
        , ssGetMimeType = \file ->
          if takeExtension (T.unpack (fromPiece file.fileName)) == "" then do
            htmlExists <- doesFileExist $ path </> T.unpack (fromPiece file.fileName) <.> "html"
            if htmlExists then pure "text/html" else settings.ssGetMimeType file
          else settings.ssGetMimeType file
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

jsonTagsCtx :: Context a
jsonTagsCtx = field "tags" \(Item id _) -> do
  meta <- getMetadata id
  pure . B.unpack . Aeson.encode . fromMaybe [] $ lookupStringList "tags" meta

data PostList
  = Tag String
  | Only Int
  | All

draftsPattern :: Pattern
draftsPattern = "drafts/*.md" .||. "drafts/*.lhs"

postsPattern :: Pattern
postsPattern = "posts/*.md" .||. "posts/*.lhs"

resumePattern :: Pattern
resumePattern = "resume/*.tex" .||. "resume/*.sty"

tagPageEntry :: String
tagPageEntry = [r|
  <h2 id="$tag$"><a href="#$tag$">#$tag$</a></h2>
  $partial("templates/post-list.html")$
|]

main :: IO ()
main = do
  hakyllArgs <- defaultParser hakyllConfig

  let
    isWatch :: Bool
    isWatch =
      case optCommand hakyllArgs of
        Watch {} -> True; _ -> False

    allPostsPattern :: Pattern
    allPostsPattern
      | isWatch   = postsPattern .||. draftsPattern
      | otherwise = postsPattern

    postList :: PostList -> Compiler [Item String]
    postList p = do
      posts <- recentFirst =<< loadAll allPostsPattern
      case p of
        All        -> pure posts
        Only n     -> pure $ take n posts
        Tag tag ->
          filterM (\(Item id _) -> elem tag <$> getTags id) posts

  hakyllWithArgs hakyllConfig hakyllArgs do
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

    match allPostsPattern do
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

    -- NOTE: even though these are not in the final site (no `route`),
    -- we need to use `copyFileCompiler` so they are in the store and
    -- dependency tracking works correctly
    match resumePattern do
      compile copyFileCompiler

    resumeDeps <- makePatternDependency resumePattern
    rulesExtraDependencies [resumeDeps] do
      create ["static/resume.pdf"] do
        route idRoute

        when isWatch do
          compile $ do
            _ <- unsafeCompiler $ readProcess "latexmk" [] []
            makeItem ()

    draftsDep <- makePatternDependency draftsPattern
    rulesExtraDependencies [draftsDep] do
      match "pages/index.md" do
        route $ basename `composeRoutes` setExtension "html"
        let indexCtx = listField "posts" postCtx (postList (Only 5))
              <> defaultContext
        compile $ pandocCCPre (applyAsTemplate indexCtx)
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
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
          . Map.fromList <$> getAllMetadata allPostsPattern

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
          posts <- recentFirst =<< loadAllSnapshots allPostsPattern "content"
          let feedCtx = postCtx <> bodyField "description"
          renderAtomWithTemplates feedT itemT feedConfig feedCtx posts

      create ["feed.json"] $ version "generated" do
        route idRoute
        compile do
          posts <- recentFirst =<< loadAllSnapshots allPostsPattern "content"
          feedT <- loadBody "templates/feed.json"
          itemT <- loadBody "templates/feed-item.json"
          let feedCtx = bodyField "description" <> jsonTagsCtx <> postCtx
          renderJsonWithTemplates feedT itemT feedConfig feedCtx posts
