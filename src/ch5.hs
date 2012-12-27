module Ch5 where

import qualified Data.Char as C

-- generators

sq :: Num a => [a] -> [a]
sq xs = [x^2 | x <- xs]

-- *Ch5> [(x,y) | x<-[1,2,3] , y <- [3,4]]
-- [(1,3),(1,4),(2,3),(2,4),(3,3),(3,4)]

-- concat
ccat :: [[a]] -> [a]
ccat xxs = [x | xs <- xxs, x <- xs]

-- *Ch5> ccat [[1,2,3], [3,4], [4,5]]
-- [1,2,3,3,4,4,5]
-- *Ch5> concat [[1,2,3], [3,4], [4,5]]
-- [1,2,3,3,4,4,5]

-- function that selects all the first elements from a list of pairs
ffsts :: [(a,a)] -> [a]
ffsts ps = [x | (x,_) <- ps]

-- *Ch5> ffsts [(1,2), (3,4), (5,6)]
-- [1,3,5]

lgth :: [a] -> Int
lgth xs = sum [1 | _ <-xs]

-- generator with guardian (filter)

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

-- prime 1 -- False
-- prime 2 -- True
-- prime 3 -- True
-- prime 4 -- False
-- prime 7 -- True

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

-- primes 250
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241]

-- a function find that returns the list of all values that are associated with
-- a given key in a table

find :: Eq a => a -> [(a,a)] -> [a]
find k hs = [v | (k', v) <- hs, k == k']

-- *Ch5> find 10 [(10, 20), (30, 40), (10, 10), (50, 60), (10, 100)]
-- [20,10,100]

-- zip
-- *Ch5> zip ['a', 'b', 'c' ] [3,2,1,4]
-- [('a',3),('b',2),('c',1)]

-- pairs that returns the list of all pairs of adjacent elements from a list
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- *Ch5> pairs [10, 20, 30, 40]
-- [(10,20),(20,30),(30,40)]

-- A function that decides if a list of elements of any ordered type is sorted
-- by simply checking that all pairs of adjacent elements from the list are in
-- the correct order

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

-- *Ch5> sorted [1,2,3]
-- True
-- *Ch5> sorted [1,3, 2]
-- False
-- *Ch5> sorted [1,1, 2]
-- True
-- *Ch5> sorted [1,1,3,2]
-- False

-- function that returns the list of all positions at which a value occurs
-- in a list, by pairing each element with its position, and selecting those
-- positions at which the desired value occurs
positions :: Eq a => a -> [a] -> [Int]
positions v vs = [i | (v', i) <- zip vs [1..(length vs - 1)], v == v']

-- *Ch5> positions 0 [1,2,3,4]
-- []
-- *Ch5> positions 1 [1,2,3,1,4]
-- [1,4]

-- count the numbers of lower letters
lowers :: [Char] -> Int
lowers cs = sum [1 | c <- cs, C.isLower c]

-- *Ch5> lowers "Christelle"
-- 9
