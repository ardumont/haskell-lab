module Main where

--import System.IO
import System.Environment (getArgs)
import Control.Exception (catch)

main :: IO ()
main = Control.Exception.catch countLines handlerExc

handlerExc :: IOError -> IO ()
handlerExc _ = putStrLn ("Oops! I did it again!")

countLines :: IO ()
countLines = do
  (filename:_) <- getArgs
  contents <- readFile filename
  putStrLn ((show . length . lines) contents)
