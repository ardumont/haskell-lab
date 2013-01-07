module Ch7_2 where

import qualified Data.Char as C

-- given a function that takes pair as parameter
add :: Num a => (a,a) -> a
add (x,y) = x + y

-- Given the curry function
cur :: ((a,b) -> c) -> a -> b -> c
cur f = \x -> \y -> f (x,y)

-- here are examples:
-- *Ch7_2> (cur add) 1 2
-- 3
-- *Ch7_2> ((cur add) 1) 2
-- 3

-- which can be put simply
currify :: ((a,b) -> c) -> a -> b -> c
currify f x y = f (x,y)

-- *Ch7_2> (currify add) 1 2
-- 3
-- *Ch7_2> ((currify add) 1) 2
-- 3

uncur :: (a -> b -> c) -> (a, b) -> c
uncur f = \ (x, y) -> (f x y)

-- *Ch7_2> add (1,2)
-- 3
-- *Ch7_2> (cur add) 1 2
-- 3
-- *Ch7_2> uncur (cur add) (1,2)
-- 3

-- which can be put simply
uncurrify :: (a -> b -> c) -> (a, b) -> c
uncurrify f (x, y) = f x y

-- *Ch7_2> uncurrify (currify add) (1,2)
-- 3

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

-- int2bin :: Int -> [Bit]
-- int2bin 0 = []
-- int2bin n = int2bin (n `div` 2) ++ [n `mod` 2]

-- then
int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

-- *Ch7_2> int2bin 20
-- [0,0,1,0,1]
-- *Ch7_2> int2bin 128
-- [0,0,0,0,0,0,0,1]

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

-- *Ch7_2> chop8 [1,1,0,1,0,0,1,0,1,1,0,1,0,0,1,0,1,1,0,1,0,0,1,0]
-- [[1,1,0,1,0,0,1,0],[1,1,0,1,0,0,1,0],[1,1,0,1,0,0,1,0]]

mapu :: (a -> b) -> [a] -> [b]
mapu f = unfold null (f . head) tail

-- *Ch7_2> mapu (+1) [1,2,4]
-- [2,3,5]
-- *Ch7_2> mapu even [1,2,4]
-- [False,True,True]
-- *Ch7_2> mapu int2bin [1,2,4,8,16]
-- [[1],[0,1],[0,0,1],[0,0,0,1],[0,0,0,0,1]]

iter :: (a -> a) -> a -> [a]
iter f x = x : iter f (f x)

-- *Ch7_2> take 10 (iter (+1) 10)
-- [10,11,12,13,14,15,16,17,18,19]
