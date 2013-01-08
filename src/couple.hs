module Couple where

-- unique couple (a,b) such that a*b = n
-- As the multiplication is commutative, we consider (a,b) == (b,a)

couple :: Int -> [(Int,Int)]
couple n = [(a,b) | a <- [1..n], b <- [1..a], a * b == n]
