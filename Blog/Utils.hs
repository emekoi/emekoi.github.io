module Blog.Utils
    ( module Blog.Utils
    ) where

import Blog.Config                     qualified as C
import Control.Applicative
import Control.Monad
import Data.Time.Clock
import Data.Time.Format
import Hakyll                          hiding (dateField, defaultContext,
                                        pandocCompiler, relativizeUrls,
                                        relativizeUrlsWith, tagsField, urlField)
import Hakyll                          qualified as H
import System.FilePath                 (takeDirectory, takeFileName)
import System.Process                  (readProcess)
import Text.Blaze.Html.Renderer.String qualified as H
import Text.Blaze.Html5                qualified as H
import Text.Blaze.Html5.Attributes     qualified as A
import Data.Text                       qualified as T

basename :: Routes
basename = customRoute (takeFileName . toFilePath)

defaultContext :: Context String
defaultContext =
    foldMap (uncurry constField) fields
    <> field "git-hash" (const gitHash)
    <> functionField "date" fmtDate
    <> H.defaultContext
  where
    fields =
      [ ("author", C.author)
      , ("lang", C.siteLang)
      , ("github", C.github)
      , ("site-source", C.siteSource)
      , ("site-title", C.siteTitle)
      , ("site-url", C.siteURL)
      , ("email", C.email)
      ]
    fmtDate ((parseDate -> Just date) : xs) _ = do
      let fmt = case xs of [] -> "%e %B %Y"; fmt : _ -> fmt
      pure $ formatTime defaultTimeLocale fmt date
    fmtDate _ _ = error "invalid use of date function"

parseDate :: String -> Maybe UTCTime
parseDate x = msum [parseTimeM True defaultTimeLocale fmt x | fmt <- formats]
  where
    formats =
      [ "%a, %d %b %Y %H:%M:%S %Z"
      , "%a, %d %b %Y %H:%M:%S"
      , "%Y-%m-%dT%H:%M:%S%Z"
      , "%Y-%m-%dT%H:%M:%S"
      , "%Y-%m-%d %H:%M:%S%Z"
      , "%Y-%m-%d %H:%M:%S"
      , "%Y-%m-%d"
      , "%d.%m.%Y"
      , "%B %e, %Y %l:%M %p"
      , "%B %e, %Y"
      , "%b %d, %Y"
      ]

dateField :: String -> String -> Context String
dateField key fmt = field key \(Item i _) -> do
  getMetadataField i key >>= \case
    Just (parseDate -> Just date) -> pure $ formatTime defaultTimeLocale fmt date
    Just _ -> noResult $ "Invalid date field '" ++ key ++ "' in context"
    _ -> noResult $ "Missing field '" ++ key ++ "' in context"

postCtx :: Context String
postCtx =
    dateField "published" "%Y-%m-%dT%H:%M:%S%Ez"
    <> urlField
    <> tagsField
    <> defaultContext

urlField :: Context a
urlField = field "url" $ \i -> do
    let id = itemIdentifier i
        empty' = fail $ "No route url found for item " ++ show id
    maybe empty' (toUrl . takeDirectory) <$> getRoute id

tagsField :: Context a
tagsField = field "tags" \(Item id _) -> do
  meta <- getMetadata id
  case lookupStringList "tags" meta of
    Nothing -> empty
    Just tags ->
      pure . H.renderHtml . H.toHtml $ f . ('#':) <$> tags
  where
    f x = H.li $ H.a
        H.! A.href (H.toValue $ "/tags" ++ x)
        H.! A.rel "tag" $ H.toHtml x

gitHash :: Compiler String
gitHash = unsafeCompiler . fmap (T.unpack . T.strip . T.pack) $
  readProcess "git" ["rev-parse", "--short", "master"] []

-- relativizeUrls :: Item String -> Compiler (Item String)
-- relativizeUrls item = do
--   route <- getRoute $ itemIdentifier item
--   pure $ case route of
--     Just r  -> fmap (withUrls (rel r)) item
--     Nothing -> item
--   where
--     isRel ('/' : '/' : _) = False
--     isRel ('/' : _)      = True
--     isRel _              = False
--     rel _ ('$' : x) = x
--     rel r x         = if isRel x then toBlogRoot r ++ x else x
