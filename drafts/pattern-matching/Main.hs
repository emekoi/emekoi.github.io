module Main
    ( main
    ) where

import Data.Text.IO       qualified as Text
import Parser
import System.Environment (getArgs)
import Text.Megaparsec    qualified as Mega

main :: IO ()
main = getArgs >>= \case
  [file] -> do
    input <- Text.readFile file
    case parse file input of
      Right x -> print x
      Left  x -> putStrLn (Mega.errorBundlePretty x)
  _ -> putStrLn "bad input"
