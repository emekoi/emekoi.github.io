{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

module Error (module Error, Position(..), Marker(..), IsString(..), Report(..)) where

import Control.Exception
import Data.Text         (Text)
-- import Data.Text                 qualified as Text
import Error.Diagnose
import GHC.Exts          (IsList (..), IsString (..))
import Prettyprinter     qualified as P
-- import Prettyprinter.Render.Text qualified as P

data ErrMsg where
  ErrText :: Text -> ErrMsg
  -- ErrDoc :: P.Doc a -> ErrMsg
  ErrList :: [ErrMsg] -> ErrMsg
  ErrShow :: Show s => s -> ErrMsg
  ErrPretty :: P.Pretty p => (forall a. P.Doc a -> P.Doc a) -> p -> ErrMsg

instance IsString ErrMsg where
  fromString = ErrText . fromString

instance IsList ErrMsg where
  type Item ErrMsg = ErrMsg
  fromList [x] = x
  fromList xs  = ErrList xs
  toList (ErrList l) = l
  toList e           = [e]

instance P.Pretty ErrMsg where
  pretty (ErrText txt) = P.pretty txt
  -- pretty (ErrDoc doc) = P.pretty
  --   . Text.unpack
  --   . P.renderStrict
  --   . P.layoutPretty P.defaultLayoutOptions $ doc
  pretty (ErrList xs) = P.hsep $
    P.pretty <$> xs
  pretty (ErrShow s) = P.viaShow s
  pretty (ErrPretty f p) = f $ P.pretty p

newtype Error
  = Error [Report ErrMsg]

instance Show Error where
  show = error "uncaught SemaError"

instance Exception Error

pattern Err' :: msg -> [(Position, Marker msg)] -> Report msg
pattern Err' msg xs = Err Nothing msg xs []

throwError :: ErrMsg -> [(Position, Marker ErrMsg)] -> error
throwError msg = throw . Error . (:[]) . Err' msg
