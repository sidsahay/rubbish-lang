module Main where

import Lib
import System.IO
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        let path = head args
        program <- readFile . head $ args
        let compiledString = compileToString program
        writeFile (last args) compiledString