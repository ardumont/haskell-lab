module Ch7 where

import qualified Data.Char as C

add0 :: Num a => a -> a -> a
add0 = \ x -> (\ y -> x + y)

-- *Ch7> (add0 1) 3
-- 4
-- *Ch7> map (add0 1) [1,2,3]
-- [2,3,4]

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- *Ch7> twice (add0 1) 1
-- 3
-- *Ch7> twice (*2) 10
-- 40
-- *Ch7> twice reverse [1,2,3]
-- [1,2,3]

mmap :: (a -> a) -> [a] -> [a]
mmap f xs = [f x | x <- xs]

-- *Ch7> mmap (add0 2) [1,2,3]
-- [3,4,5]

mrmap :: (a -> a) -> [a] -> [a]
mrmap _ [] = []
mrmap f (x:xs) = f x:(mrmap f xs)

-- *Ch7> mrmap (add0 2) [1,2,3]
-- [3,4,5]

mfilter :: (a -> Bool) -> [a] -> [a]
mfilter p xs = [x | x <- xs, p x]

-- *Ch7> mfilter even [1..10]
-- [2,4,6,8,10]
-- *Ch7> mfilter (/= ' ') "abc def ghi"
-- "abcdefghi"

mrfilter :: (a -> Bool) -> [a] -> [a]
mrfilter _ [] = []
mrfilter p (x:xs) | p x       = x : mrfilter p xs
                  | otherwise = mrfilter p xs

-- *Ch7> mrfilter even [1..10]
-- [2,4,6,8,10]
-- *Ch7> mrfilter (/= ' ') "abc def ghi"
-- "abcdefghi"

-- OTHER HOF

-- *Ch7> all (\ x -> even x || odd x) [1,2,3,4]
-- True
-- *Ch7> all odd [1,2,3,4]
-- False
-- *Ch7> all even [1,2,3,4]
-- False
-- *Ch7> any even [1,2,3,4]
-- True
-- *Ch7> any odd [1,2,3,4]
-- True
-- *Ch7> takeWhile C.isLower "ZNBDHFabcdefghiljkl"
-- ""
-- *Ch7> takeWhile C.isLower "abdZNBDHFabcdefghiljkl"
-- "abd"
-- *Ch7> dropWhile C.isLower "abdZNBDHFabcdefghiljkl"
-- "ZNBDHFabcdefghiljkl"

mfoldr :: (a -> b -> b) -> b -> [a] -> b
mfoldr _ v [] = v
mfoldr f v (x:xs) = f x (mfoldr f v xs)

-- *Ch7> (mfoldr (+) 1) [1,2,3,4]
-- 11
-- *Ch7> (mfoldr (*) 1) [1,2,3,4]
-- 24
-- *Ch7> (mfoldr (\ _ n -> n + 1) 0) [1,2,3,4]
-- 4
-- *Ch7> (mfoldr (\ x xs -> xs ++ [x]) []) [1,2,3,4]
-- [4,3,2,1]

addr :: [Integer] -> Integer
addr = mfoldr (+) 0

-- *Ch7> addr [1,2,3,4]
-- 10
-- *Ch7> addr [10,2,30,4]
-- 46

productr :: [Integer] -> Integer
productr = mfoldr (*) 1

-- *Ch7> productr [1,2,3]
-- 6
-- *Ch7> productr [1,10,3]
-- 30
-- *Ch7> productr [21,10,3]
-- 630

orr :: [Bool] -> Bool
orr = mfoldr (||) False

-- *Ch7> orr [False, False, False]
-- False
-- *Ch7> orr [True, False, False]
-- True

andr :: [Bool] -> Bool
andr = mfoldr (&&) True

-- *Ch7> andr [True, True, True, True]
-- True
-- *Ch7> andr [True, False, True]
-- False

revr :: [a] -> [a]
revr = mfoldr (\ x xs -> xs ++ [x]) []

-- *Ch7> revr [12,20,1,0]
-- [0,1,20,12]

mfoldl :: (a -> b -> b) -> b -> [a] -> b
mfoldl _ v [] = v
mfoldl f v (x:xs) = (mfoldl f (f x v) xs)

-- *Ch7> (mfoldl (+) 1) [1,2,3,4]
-- 11
-- *Ch7> (mfoldl (*) 1) [1,2,3,4]
-- 24
-- *Ch7> (mfoldl (\ _ n -> n + 1) 0) [1,2,3,4]
-- 4
-- *Ch7> (mfoldl (\ x xs -> x:xs) []) [1,2,3,4]
-- [4,3,2,1]

addl :: [Integer] -> Integer
addl = mfoldl (+) 0

-- *Ch7> addl [1,2,3,4]
-- 10
-- *Ch7> addl [10,2,30,4]
-- 46

productl :: [Integer] -> Integer
productl = mfoldl (*) 1

-- *Ch7> productl [1,2,3]
-- 6
-- *Ch7> productl [1,10,3]
-- 30
-- *Ch7> productl [21,10,3]
-- 630

orl :: [Bool] -> Bool
orl = mfoldl (||) False

-- *Ch7> orl [False, False, False]
-- False
-- *Ch7> orl [True, False, False]
-- True

andl :: [Bool] -> Bool
andl = mfoldl (&&) True

-- *Ch7> andl [True, True, True, True]
-- True
-- *Ch7> andl [True, False, True]
-- False

revl :: [a] -> [a]
revl = mfoldl (\ x xs -> x : xs) []

-- *Ch7> revl [12,20,1,0]
-- [0,1,20,12]

mcomp :: (b -> c) -> (a -> b) -> a -> c
mcomp f g = \ x -> f (g x)

modd :: Integer -> Bool
modd = not . even

-- *ch7> map modd [1..5]
-- [True,False,True,False,True]

twice2 :: (a -> a) -> a -> a
twice2 f = f . f

-- *Ch7> twice2 (add0 10) 100
-- 120

sumsqeven :: [Integer] -> Integer
sumsqeven = sum . map (\ x -> x ^ 2) . filter even

-- *Ch7> sumsqeven [1..10]
-- 220

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

arr2Int :: [Integer] -> Integer
arr2Int xs = sum [ x * y | (x, y) <- zip (reverse xs) [10 ^ n | n <- [0..(length xs - 1)] ] ]
