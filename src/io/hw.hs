module Main where

import Control.Monad (when)

main :: IO ()
main = do putStrLn "Hello, what's your name?"
          n <- getLine
          putStrLn $ "Hello " ++ reverse n

infiniteRead :: IO ()
infiniteRead = do line <- getLine
                  if null line
                    then return ()
                    else do putStrLn $ reverseWords line
                            infiniteRead

reverseWords :: String -> String
reverseWords = unwords . reverse . words

-- *Main> reverseWords "this is to be reversed"
-- "reversed be to is this"

readL :: IO ()
readL =
  do x <- getChar
     when (x /= '\n')
       (do putChar x
           readL)
