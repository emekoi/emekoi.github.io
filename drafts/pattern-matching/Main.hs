module Main
    ( main
    ) where

import Control.Exception
import Data.ByteString.Lazy.Char8 qualified as BS
import Elab
import Error
import Error.Diagnose
import Lexer
import Parser
import Prettyprinter              qualified as P
import Prettyprinter.Render.Text  qualified as P
import System.Environment         qualified as System
import System.Exit                qualified as System

main :: IO ()
main = do
  (input, file) <- System.getArgs >>= \case
    []       -> (,"<stdin>") <$> BS.getContents
    file : _ -> (,file) <$> BS.readFile file
  let diagFile = addFile mempty file (BS.unpack input)
  handle (handleErr diagFile) do
    let ds = runAlex' file input parse
    ds <- runElab file (elaborate ds)
    P.putDoc $ P.concatWith (\x y -> x <> P.line <> P.line <> y) (P.pretty <$> ds)
      <> P.line
  where
    handleErr file (Error err) = do
      printDiagnostic stderr WithUnicode (TabSize 2) defaultStyle (foldl addReport file err)
      System.exitFailure
