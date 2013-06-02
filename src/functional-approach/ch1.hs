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

fn :: [Int] -> [Int]
fn l = reverse (f' l [])
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

foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' _ x []     = x
foldr'' f x (y:ys) = f y $ foldr'' f x ys

-- *Ch1> foldr'' (+) 0 [1..10]
-- 55
-- *Ch1> foldr (+) 0 [1..10]
-- 55
-- *Ch1> foldr'' (-) 0 [1..10]
-- -5
-- *Ch1> foldr (-) 0 [1..10]
-- -5

foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' _ x [] = x
foldl'' f x (y:ys) = foldl'' f (f x y) ys

-- *Ch1> foldl (+) 0 [1..10]
-- 55
-- *Ch1> foldl'' (+) 0 [1..10]
-- 55

tmp :: (Integer, Integer)
tmp = (foldr f 0 l, foldl f 0 l)
  where l = [6,9,8,3,10]
        f x y = (x+y) `div` 2

-- *Ch1> tmp
-- (6,7)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

string2int' :: String -> Int
string2int' xs = sum [ compose (* u) digitToInt x | (u, x) <- zip unit (reverse xs)]
                 where unit = iterate (* 10) 1

cube :: Num a => a -> a
cube x = x * x * x

maxi :: (Ord a) => a -> a -> a
maxi x y | x >= y = x
         | otherwise = y

sumAtoB :: (Num a, Enum a) => a -> a -> a
sumAtoB a b = sum [a..b]

transpose3 :: [[a]] -> [[a]]
transpose3 xs = map (\ n -> map (!! n) xs) [0..2]

-- *Ch1> transpose3 [[1,2,3], [4,5,6], [7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]

transpose :: [[a]] -> [[a]]
transpose xs = map (\ n -> map (!! n) xs) [0..l]
               where l = length xs - 1

-- contract
-- *Ch1> Data.List.transpose [[1,2,3], [4,5,6], [7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]
-- *Ch1> transpose [[1,2,3], [4,5,6], [7,8,9]]
-- [[1,4,7],[2,5,8],[3,6,9]]
-- *Ch1> transpose [[1,2,3,4], [5,6,7,8], [9,10,11,12], [13,14,15,16]]
-- [[1,5,9,13],[2,6,10,14],[3,7,11,15],[4,8,12,16]]
