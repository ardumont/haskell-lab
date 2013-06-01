module Ch1 where

import Data.Char (digitToInt)

fact :: Int -> Int
fact n
  | n < 0     = -1
  | n == 0    = 1
  | otherwise = n * fact (n-1)

-- *Ch1> fact 0
-- 1
-- *Ch1> fact 1
-- 1
-- *Ch1> fact 5
-- 120
-- *Ch1> fact (-10)
-- -1

f :: [Int] -> [Int]
f l = reverse (f' l [])
      where f' [] r     = r
            f' (x:xs) r = (2*x) : (f' xs r)

avg :: [Int] -> Int
avg xs | null xs   = 0
       | otherwise = (sum xs) `div` (length xs)

-- *Ch1> avg [1..10]
-- 5
-- *Ch1> avg []
-- 0

mdl :: [a] -> Maybe a
mdl xs | null xs   = Nothing
       | otherwise = Just (xs !! p)
                     where p = ((subtract 1) . (`div` 2) . length) xs

-- *Ch1> mdl [1..10]
-- Just 5
-- *Ch1> mdl [1..20]
-- Just 10
-- *Ch1> mdl []
-- Nothing

neg :: [Int] -> Int
neg xs = sum [1 | x <- xs, x < 0 ]

-- *Ch1> neg [1, -9, 5, 4, -6, 0]
-- 2
-- *Ch1> neg [1, -9, -5, 4, -6, 0]
-- 3

rep :: Int -> [Int]
rep n = [ y | x <- [1..n], y <- replicate x x]

-- *Ch1> rep 0
-- []
-- *Ch1> rep 1
-- [1]
-- *Ch1> rep 2
-- [1,2,2]
-- *Ch1> rep 3
-- [1,2,2,3,3,3]
-- *Ch1> rep 4
-- [1,2,2,3,3,3,4,4,4,4]

string2int :: String -> Int
string2int xs = sum [ ((* u) . digitToInt) x | (u, x) <- zip unit (reverse xs)]
                where unit = iterate (* 10) 1

-- *Ch1> string2int "123"
-- 123
-- *Ch1> string2int "3434"
-- 3434
-- *Ch1> string2int "3454"
-- 3454
-- *Ch1> string2int "76"
-- 76
