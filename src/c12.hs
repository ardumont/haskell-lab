module C12 where

fibs :: [Int]
fibs = fibo [0,1]
       where
         fibo (x:y:xs) = x:fibo (y:x+y:xs)
