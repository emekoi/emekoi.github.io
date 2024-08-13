{-# LANGUAGE CPP #-}

module Main
    ( main
    ) where

import Data.Text.IO         qualified as Text
import Lang qualified
import Parser
import System.Environment   (getArgs)
import Text.Megaparsec      qualified as Mega

#if defined(JSON_ENABLE)
import Data.Aeson           qualified as Aeson
import Data.Aeson.Encoding  qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Orphans              ()
#endif

main :: IO ()
main = getArgs >>= \case
  file : _rest -> do
    input <- Text.readFile file
    case parse file input of
      Left  x -> putStrLn (Mega.errorBundlePretty x)
#if defined(JSON_ENABLE)
      Right x | "-json" `elem` _rest ->
        BSL.putStr . Aeson.encodingToLazyByteString $ Aeson.toEncoding x
#endif
      Right x -> do
        m <- Lang.elaborate x
        mapM_ (putStrLn . \(x, Lang.Var v t) -> show v ++ " : " ++ show t ++ " = " ++ Lang.dump x) m.terms
  _ -> putStrLn "bad input"
