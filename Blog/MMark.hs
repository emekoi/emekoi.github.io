module Blog.MMark
    ( module Blog.MMark
    ) where

import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.IO          as Text
import qualified Data.Text.Lazy.IO     as TextL
import qualified Lucid
import qualified Text.Megaparsec.Error as Mega
import qualified Text.MMark            as MMark

render :: Text -> IO ()
render (Text.unpack -> input) = do
  txt <- Text.readFile input -- (1)
  case MMark.parse input txt of -- (2)
    Left errs -> putStrLn (Mega.errorBundlePretty errs) -- (3)
    Right r -> TextL.writeFile "output.html" -- (6)
      . Lucid.renderText -- (5)
      . MMark.render -- (4)
      $ r
