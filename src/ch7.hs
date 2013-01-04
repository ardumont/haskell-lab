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
