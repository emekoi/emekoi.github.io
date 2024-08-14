module Main
    ( main
    ) where

import Data.Text                  qualified as Text
import Data.Text.IO               qualified as Text
import Parser qualified
import System.Environment         (getArgs)
import Text.Megaparsec            qualified as Mega
import Control.Exception          (handle)
import Control.Monad
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Reader
import Poly qualified

polyMain :: [(Text.Text, Maybe Poly.RType, Poly.Expr)] -> IO ()
polyMain xs = Poly.runCheck $ forM_ xs \(x, t, e) -> wrap do
  t <- case t of
    Just t  -> Poly.exprTopCheck e t
    Nothing -> Poly.exprTopInfer e
  s <- Poly.typePrint' t
  liftIO . Text.putStrLn $ x <> " : " <> s
  where
    wrap m = ReaderT $
      handle @Poly.TypeError (\x -> print x *> putChar '\n') . runReaderT m

main :: IO ()
main = do
  (file, input) <- getArgs >>= \case
    file : _ -> (file, ) <$> Text.readFile file
    _ -> ("<stdin>",) <$> Text.getContents

  case Mega.runParser Parser.parser file input of
    Left  x  -> putStrLn (Mega.errorBundlePretty x)
    Right xs -> polyMain xs

  pure ()
