module Main
    ( main
    ) where

import Control.Exception          (handle)
import Control.Monad
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Reader
import Data.Text.IO               qualified as Text
import Infer qualified
import Parser qualified
import Syntax
import System.Environment         (getArgs)
import Text.Megaparsec            qualified as Mega

polyMain :: [(Name, Maybe RType, Expr)] -> IO ()
polyMain xs = runCheck $ forM_ xs \(x, t, e) -> wrap do
  s <- display =<< case t of
    Just t  -> Infer.exprTopCheck e t
    Nothing -> Infer.exprTopInfer e
  liftIO . Text.putStrLn $ x <> " : " <> s
  where
    wrap m = ReaderT $
      handle @Infer.TypeError (\x -> print x *> putChar '\n') . runReaderT m

main :: IO ()
main = do
  (file, input) <- getArgs >>= \case
    file : _ -> (file, ) <$> Text.readFile file
    _ -> ("<stdin>",) <$> Text.getContents

  case Mega.runParser Parser.parser file input of
    Left  x  -> putStrLn (Mega.errorBundlePretty x)
    Right xs -> polyMain xs

  pure ()
