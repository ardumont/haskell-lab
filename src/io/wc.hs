module Main where

--import System.IO
import System.Environment (getArgs)
import Control.Exception (catch)
import System.IO.Error (isDoesNotExistError)

main :: IO ()
main = Control.Exception.catch countLines handlerExc

handlerExc :: IOError -> IO ()
handlerExc e
  | isDoesNotExistError e = putStrLn "The file does not exist!"
  | otherwise             = ioError e

countLines :: IO ()
countLines = do
  (filename:_) <- getArgs
  contents <- readFile filename
  putStrLn ((show . length . lines) contents)
