module Blog.Util
    ( jsonInsert
    , liftAction
    , titleSlug
    ) where

import Data.Aeson
import Data.Aeson.KeyMap  qualified as Aeson
import Data.Char          qualified as Char
import Data.Text          (Text)
import Data.Text          qualified as Text
import Data.Text.ICU      qualified as ICU
import Data.Text.ICU.Char qualified as ICU
import Development.Shake

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
titleSlug = f . ICU.nfd
  where
    g c | ICU.property ICU.Diacritic c = ""
    g c | Char.isAscii c && Char.isAlphaNum c = Text.singleton $ Char.toLower c
    g c | Just c' <- latin1Replace c = c'
    g _ = " "
    f = Text.intercalate "-"
      . Text.words
      . Text.concatMap g

-- | convert an Action r to a Rules (Action r) with sharing
liftAction :: Action a -> Rules (Action a)
liftAction m = ($ ()) <$> newCache (`seq` m)

jsonInsert :: ToJSON v =>  Key -> v -> Value -> Value
jsonInsert key val (Object obj) =
  Object (Aeson.insert key (toJSON val) obj)
jsonInsert _ _ x = x
