module Main
    ( main
    ) where

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print
