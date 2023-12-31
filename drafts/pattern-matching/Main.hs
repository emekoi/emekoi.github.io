module Main
    ( main
    ) where

import Control.Exception
import Data.ByteString.Lazy.Char8 qualified as BS
import Error.Diagnose
import Lexer
import Parser
import Prettyprinter              qualified as P
import Prettyprinter.Render.Text  qualified as P
import Sema
import System.Environment         qualified as System
import System.Exit                qualified as System
import System.IO                  qualified as System

semaMain :: Module -> IO ()
semaMain ds =
  let (ds', _) = runSema $ collectDataCons ds in
    P.putDoc $ P.concatWith (\x y -> x <> P.line <> y) (P.pretty <$> ds')

main :: IO ()
main = do
  (input, file) <- System.getArgs >>= \case
    [] -> (, "<stdin>") <$> BS.getContents
    file : _ -> (, file) <$> BS.readFile file
  let diag = addFile def file (BS.unpack input)
  case runAlex' file input parse of
    Left err -> do
      System.hPutStrLn System.stderr err
      System.exitFailure
    Right ds ->
      semaMain ds `catch` \(SemaError err) -> do
        printDiagnostic stderr True True 2 defaultStyle (addReport diag err)
