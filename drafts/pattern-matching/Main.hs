module Main
    ( main
    ) where

import Data.Aeson           qualified as Aeson
import Data.Aeson.Encoding  qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text.IO         qualified as Text
import Lang qualified
import Orphans              ()
import Parser
import System.Environment   (getArgs)
import Text.Megaparsec      qualified as Mega

main :: IO ()
main = getArgs >>= \case
  file : rest -> do
    input <- Text.readFile file
    case parse file input of
      Left  x -> putStrLn (Mega.errorBundlePretty x)
      Right x | "-json" `elem` rest ->
        BSL.putStr . Aeson.encodingToLazyByteString $ Aeson.toEncoding x
      Right x -> do
        Lang.rawModule x >>= print
  _ -> putStrLn "bad input"
