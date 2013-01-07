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
int2bin0 :: Int -> [Bit]
int2bin0 = unfold (== 0) (`mod` 2) (`div` 2)

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

iter2 :: (a -> a) -> a -> [a]
iter2 f = unfold (\ _ -> False) id f

-- *Ch7_2> take 10 (iter2 (+1) 10)
-- [10,11,12,13,14,15,16,17,18,19]
-- *Ch7_2> take 10 (iter2 (+2) 0)
-- [0,2,4,6,8,10,12,14,16,18]

iter3 :: (a -> a) -> a -> [a]
iter3 f = unfold (const False) id f

-- *Ch7_2> take 10 (iter3 (*2) 0)
-- [0,2,4,6,8,10,12,14,16,18]

bin2int :: [Bit] -> Int
bin2int = foldl (\ x y -> x * 2 + y) 0

-- *Ch7> bin2int [0,0,0,0,1,1,0,1]
-- 13

-- make sure a list of bits is of same length 8
make8 :: [Bit] -> [Bit]
make8 bits = reverse (take 8 (reverse bits ++ repeat 0))

-- *Ch7> make8 [1,1,0,1]
-- [0,0,0,0,1,1,0,1]
-- *Ch7> bin2int (make8 [1,1,0,1])
-- 13

parbit :: [Bit] -> [Bit]
parbit xs | even (sum xs) = xs ++ [0] -- even number of ones
          | otherwise     = xs ++ [1] -- odd number of ones

-- *Ch7_2> parbit [1,1,0]
-- [1,1,0,0]
-- *Ch7_2> parbit [1,1,1]
-- [1,1,1,1]

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = int2bin (n `div` 2) ++ [n `mod` 2]

encode :: String -> [Bit]
encode = concat . map (parbit . make8 . int2bin . C.ord)

checkParbit :: [Bit] -> [Bit]
checkParbit xs | (parbit (take 8 xs) == xs) = (take 8 xs)
               | otherwise = error "Not ok"

-- *Ch7_2> checkParbit (encode "a")
-- [0,1,0,0,0,0,1,1]
-- *Ch7_2> checkParbit [0,1,0,0,0,0,1,0]
-- *** Exception: Not ok

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (take 9) (drop 9)

decode :: [Bit] -> String
decode = map (C.chr . bin2int . checkParbit) . chop9

channel :: a -> a
channel = id

transmit :: String -> String
transmit = decode . channel . encode

-- *Ch7_2> transmit "haskell is great"
-- "haskell is great"
