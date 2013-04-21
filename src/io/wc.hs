module Main where

import System.Environment

main :: IO ()
main = do (filename:_) <- getArgs
          contents <- readFile filename
          putStrLn ((show . length . lines) contents)
