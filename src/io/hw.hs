module Main where

main :: IO ()
main = do putStrLn "Hello, what's your name?"
          n <- getLine
          putStrLn $ "Hello " ++ reverse n
