module Main where

--import System.IO
import System.Environment (getArgs)
import System.Directory (doesFileExist)

main :: IO ()
main = do (filename:_) <- getArgs
          fileExists <- doesFileExist filename
          if fileExists
             then (do contents <- readFile filename
                      putStrLn ((show . length . lines) contents))
                  else putStrLn ("'" ++ filename ++ "' does not exists!")
