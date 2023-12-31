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
  let diag = addFile mempty file (BS.unpack input)
  case runAlex' file input parse of
    Left err -> do
      System.hPutStrLn System.stderr err
      System.exitFailure
    Right ds ->
      semaMain ds `catch` \(SemaError err) -> do
        printDiagnostic stderr WithUnicode (TabSize 2) defaultStyle (addReport diag err)

-- main = do
--   let
--     beautifulExample =
--       Err
--         Nothing
--         ("Could not deduce constraint 'Num(a)' from the current context" :: String)
--         [ (Position (1, 25) (2, 6) "somefile.zc", This "While applying function '+'")

--           (Position (1, 11) (1, 16) "somefile.zc", Where "'x' is supposed to have type 'a'"),
--           (Position (1, 8) (1, 9) "somefile.zc", Where "type 'a' is bound here without constraints")
--         ]
--         ["Adding 'Num(a)' to the list of constraints may solve this problem."]
--         -- ^^^^ This is a 'Note' not a 'Hint', as specified by its 'IsString' instance

--   -- Create the diagnostic
--   let diagnostic  = addFile mempty "somefile.zc" "let id<a>(x : a) : a := x\n  + 1"
--   let diagnostic' = addReport diagnostic beautifulExample

--   -- Print with unicode characters, and the default (colorful) style
--   printDiagnostic stdout WithUnicode (TabSize 4) defaultStyle diagnostic'
