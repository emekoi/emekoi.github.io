module Blog.Slug
    ( module Blog.Slug
    ) where

import qualified Data.Char          as Char
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.ICU      as I
import qualified Data.Text.ICU.Char as I

latin1Replace :: Char -> Maybe Text
latin1Replace '\x0c6' = Just "ae"
latin1Replace '\x0e6' = Just "ae"
latin1Replace '\x0d0' = Just "d"
latin1Replace '\x0f0' = Just "d"
latin1Replace '\x0d8' = Just "o"
latin1Replace '\x0f8' = Just "o"
latin1Replace '\x0de' = Just "th"
latin1Replace '\x0fe' = Just "th"
latin1Replace '\x0df' = Just "ss"
latin1Replace '\x131' = Just "i"
latin1Replace '\x141' = Just "oe"
latin1Replace '\x142' = Just "oe"
latin1Replace '\x152' = Just "oe"
latin1Replace '\x153' = Just "oe"
latin1Replace '\x191' = Just "f"
latin1Replace '\x192' = Just "f"
latin1Replace _       = Nothing

titleSlug :: Text -> Text
titleSlug = f . I.nfd
  where
    g c | I.property I.Diacritic c = ""
    g c | Char.isAscii c && Char.isAlphaNum c = T.singleton $ Char.toLower c
    g c | Just c' <- latin1Replace c = c'
    g _ = " "
    f = T.intercalate "-"
      . T.words
      . T.concatMap g
