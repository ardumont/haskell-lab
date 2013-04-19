module Main where

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

breakWords :: String -> [String]
breakWords [] = []
breakWords s = case (break (== ' ') s) of
  (x, xs) -> x : (breakWords . drop 1) xs

-- *Main> breakWords "this is a test"
-- ["this","is","a","test"]

reverseWords :: String -> String
reverseWords = unwords . reverse . breakWords

-- *Main> reverseWords "this is to be reversed"
-- "reversed be to is this"
