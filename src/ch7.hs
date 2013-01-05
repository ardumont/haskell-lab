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

mfoldl :: (a -> b -> a) -> a -> [b] -> a
mfoldl _ v [] = v
mfoldl f v (x:xs) = (mfoldl f (f v x) xs)

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
revl = mfoldl (\ xs x -> x : xs) []

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

dec2Int :: [Int] -> Int
dec2Int xs = sum [x * y | (x, y) <- zip (reverse xs) (iterate (*10) 1)]

type Bit = Int

-- from the left 10 -> 2; 11 -> 3; etc...

bin2Integer :: [Bit] -> Int
bin2Integer xs = sum [x * y | (x, y) <- zip (reverse xs) (iterate (*2) 1)]

-- *Ch7> map bin2Int [[1,1,1,1], [1,1,1], [1,1,0,1]]
-- [15,7,13]

-- Indeed, a binary conversion can be rewritten like this
-- [d, c, b, a]
-- 8*d + 4*c + 2*b + 1*a
-- (4d + 2c + b) * 2 + a
-- (((2d + c) * 2) + b) * 2 + a
-- ((((d + 0) * 2 + c) * 2) + b) * 2 + a

bin2int :: [Bit] -> Int
bin2int = mfoldl (\ x y -> x * 2 + y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = int2bin (n `div` 2) ++ [n `mod` 2]

-- *Ch7> int2bin 13
-- [1,0,1,1]
-- *Ch7> int2bin 90
-- [0,1,0,1,1,0,1]
-- *Ch7> int2bin 127
-- [1,1,1,1,1,1,1]
-- *Ch7> int2bin 128
-- [0,0,0,0,0,0,0,1]
-- *Ch7> int2bin 128
-- [0,0,0,0,0,0,0,1]

-- make sure a list of bits is of same length 8
make8 :: [Bit] -> [Bit]
make8 bits = reverse (take 8 (reverse bits ++ repeat 0))

-- *Ch7> make8 [1,1,0,1]
-- [0,0,0,0,1,1,0,1]
-- *Ch7> bin2int (make8 [1,1,0,1])
-- 13
-- *Ch7> bin2int [0,0,0,0,1,1,0,1]
-- 13

-- make a list of infinite bits a list of list of 8 bits
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : (chop8 (drop 8 bits))

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . C.ord)

decode :: [Bit] -> String
decode = map (C.chr . bin2int) . chop8

channel :: a -> a
channel = id

transmit :: String -> String
transmit = decode . channel . encode

------------------ Exercise

all1 :: (a -> Bool) -> [a] -> Bool
all1 p = and . map p

-- *Ch7> all even1 [1,2,4]
-- False
-- *Ch7> all even1 [0,2,4]
-- True

any1 :: (a -> Bool) -> [a] -> Bool
any1 p = or . map p

-- *Ch7> any1 odd [0,2,1]
-- True
-- *Ch7> any1 odd [0,2,4]
-- False

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x:xs) | p x       = x : takeWhile1 p xs
                    | otherwise = []

-- *Ch7> takeWhile1 even [0,2,3,4,5,6]
-- [0,2]
-- *Ch7> takeWhile1 even []
-- []

dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 p (x:xs) | p x        = dropWhile1 p xs
                    | otherwise  = x : xs

-- *Ch7> dropWhile1 odd [1,3,5,7,0,2,3,4,5,6]
-- [0,2,3,4,5,6]
-- *Ch7> dropWhile1 odd []
-- []

mfmap :: (a -> b) -> [a] -> [b]
mfmap f = foldr (\ x xs -> f x : xs) []

-- *ch7> mfmap even [1,2,3]
-- [False,True,False]
-- *Ch7> mfmap (*3) [1,2,3]
-- [3,6,9]

mffilter :: (a -> Bool) -> [a] -> [a]
mffilter p = foldr (\ x xs -> if (p x) then x : xs else xs) []

-- Indeed, a decimal conversion can be rewritten like this
-- Given a list of int [d,c,b,a] representing a 4-digit number
-- It's decomposition can be written like this:
-- [d, c, b, a]
-- 1000*d + 100*c + 10*b + 1*a
-- (100 * d + 10 * c + b) * 10 + a
-- (((10 * d + c) * 10) + b) * 10 + a
-- ((((d + 0) * 10 + c) * 10) + b) * 10 + a

dec2int :: [Int] -> Int
dec2int = foldl (\ x y -> x * 10 + y) 0
