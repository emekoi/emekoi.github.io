module Error (module Error, Position(..), Marker(..), IsString(..)) where

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
  = Error (Report ErrMsg)

instance Show Error where
  show = error "uncaught SemaError"

instance Exception Error

throwError :: ErrMsg -> [(Position, Marker ErrMsg)] -> error
throwError msg xs = throw . Error $ Err Nothing msg xs []
  -- where
  --   f (fp, Range (AlexPn _ l1 c1) (AlexPn _ l2 c2), d) =
  --     (Position (l1, c1) (l2, c2) fp, d)

throwError' :: [P.Doc a] -> [(Position, Marker ErrMsg)] -> error
throwError' = throwError . ErrDoc . P.hsep
