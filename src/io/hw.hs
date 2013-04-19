module Main where

main :: IO ()
main = do putStrLn "Hello, what's your name?"
          n <- getLine
          putStrLn $ "Hello " ++ reverse n

infiniteRead :: IO ()
infiniteRead = do line <- getLine
                  if line == ""
                    then return ()
                    else do putStrLn $ reverse line
                            infiniteRead
