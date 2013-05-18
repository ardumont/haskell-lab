module Main where

--import System.IO
import System.Environment (getArgs)
import Control.Exception (catch)
import System.IO.Error (isDoesNotExistError,
                        ioeGetFileName)

main :: IO ()
main = Control.Exception.catch countLines handlerExc

handlerExc :: IOError -> IO ()
handlerExc e
  | isDoesNotExistError e =
    case ioeGetFileName e of
      Just file -> putStrLn $ "'" ++ file ++ "' does not exist!"
      Nothing   -> putStrLn "Oops, the file does not exist!"
  | otherwise             = ioError e

countLines :: IO ()
countLines = do
  (filename:_) <- getArgs
  contents <- readFile filename
  putStrLn $ (show . length . lines) contents
