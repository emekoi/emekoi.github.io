module Error (module Error, Position(..), Marker(..), IsString(..), Report(..)) where

import Control.Exception
import Data.Text                 (Text)
import Data.Text                 qualified as Text
import Error.Diagnose
import GHC.Exts                  (IsString (..))
import Prettyprinter             qualified as P
import Prettyprinter.Render.Text qualified as P

data ErrMsg where
  ErrText :: Text -> ErrMsg
  ErrDoc :: P.Doc a -> ErrMsg

instance IsString ErrMsg where
  fromString = ErrText . fromString

instance P.Pretty ErrMsg where
  pretty (ErrText txt) = P.pretty txt
  pretty (ErrDoc doc) = P.pretty
    . Text.unpack
    . P.renderStrict
    . P.layoutPretty P.defaultLayoutOptions $ doc

newtype Error
  = Error [Report ErrMsg]

instance Show Error where
  show = error "uncaught SemaError"

instance Exception Error

pattern Err' :: msg -> [(Position, Marker msg)] -> Report msg
pattern Err' msg xs = Err Nothing msg xs []

throwError :: ErrMsg -> [(Position, Marker ErrMsg)] -> error
throwError msg = throw . Error . (:[]) . Err' msg

throwError' :: [P.Doc a] -> [(Position, Marker ErrMsg)] -> error
throwError' = throwError . ErrDoc . P.hsep
