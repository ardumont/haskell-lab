module Ch1 where

fact :: Int -> Int
fact 1 = 1
fact n = n * fact (n-1)
