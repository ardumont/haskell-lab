module Fibo where

fib :: [Int]
fib = 1:1:[x+y | (x,y) <- zip fib (tail fib)]

fib' :: [Int]
fib' = fibonacci [1,1]
       where
         fibonacci (x:y:xs) = x:fibonacci(y:x+y:xs)

firstFibo :: Int -> Int
firstFibo n = head $ dropWhile (<= n) fib
