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
lowers :: String -> Int
lowers cs = sum [1 | c <- cs, C.isLower c]

-- *Ch5> lowers "Christelle"
-- 9

-- count the number of occurences of a particular characters
count :: Char -> String -> Int
count c cs = sum [1 | c' <- cs, c' == c]

-- *Ch5> count 'c' "Christelle"
-- 0
-- *Ch5> count 'C' "Christelle"
-- 1
-- *Ch5> count 'e' "Christelle"
-- 2
-- *Ch5> count 'r' "Christelle"
-- 1
-- *Ch5> count 'l' "Christelle"
-- 2
-- *Ch5> count 's' "Mississipi"
-- 4

-- caesar cipher
-- only with lower case characters

-- converts a lower-case letter between ’a’ and ’z’ into the corresponding
-- integer between 0 and 25,

let2int :: Char -> Int
let2int c = C.ord c - C.ord 'a'

-- *Ch5> map let2int ['a'..'z']
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]

int2let :: Int -> Char
int2let l = C.chr (C.ord 'a' + l)

-- *Ch5> map int2let [0..25]
-- "abcdefghijklmnopqrstuvwxyz"

shift :: Int -> Char -> Char
shift n c | C.isLower c = int2let ((n + let2int c) `mod` 26)
          | otherwise = c

-- *Ch5> shift 3 'z'
-- 'c'
-- *Ch5> shift 3 'a'
-- 'd'
-- *Ch5> shift (-3) 'c'
-- 'z'

encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]

-- *Ch5> encode 3 "abc"
-- "def"
-- *Ch5> encode 3 "haskell is fun to learn!"
-- "kdvnhoo lv ixq wr ohduq!"
-- *Ch5> encode (-3) "kdvnhoo lv ixq wr ohduq!"
-- "haskell is fun to learn!"

-- frequency table
table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- calculates the percentage of one integer with respect to another
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- function that returns a frequency table for any string
freqs :: String -> [Float]
freqs cs = [percent (count c cs) n | c <- ['a'..'z']]
           where n = (lowers cs)

-- > freqs "abbcccddddeeeee"
-- [6.666667,13.333334,20.0,26.666668,33.333336,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ ((o - e) ^ 2) / e | (o, e) <- zip os es]

-- rotates the elements of a list n places to the left, wrapping around
-- at the start of the list
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- *Ch5> rotate 1 [1,2,3,4,5]
-- [2,3,4,5,1]
-- *Ch5> rotate 2 [1,2,3,4,5]
-- [3,4,5,1,2]
-- *Ch5> rotate 3 [1,2,3,4,5]
-- [4,5,1,2,3]

crack :: String -> String
crack xs = encode (- factor) xs
           where
             factor = head (positions (minimum chitab) chitab)
             chitab = [ chisqr (rotate n t) table | n <- [1..25]]
             t = freqs xs

-- *Ch5> encode 3 "haskell is fun"
-- "kdvnhoo lv ixq"
-- *Ch5> crack "kdvnhoo lv ixq"
-- "haskell is fun"
-- *Ch5> encode 10 "haskell is fun"
-- "rkcuovv sc pex"
-- *Ch5> crack "rkcuovv sc pex"
-- "haskell is fun"

-- Using a list comprehension, give an expression that calculates the
-- sum *1^2 + 2^2 + . . . 100^2* of the first one hundred integer squares.
ssqr :: Int -> Int
ssqr n = sum [ x^2 | x <- [1..n]]

-- In a similar way to the function *length*, show how the library function *replicate :: Int → a → [ a ]*
-- that produces a list of identical elements can be defined using a list comprehension.
replica :: Int -> a -> [a]
replica n x = [x | _ <- [1..n] ]

-- *Ch5> replica 10 'a'
-- "aaaaaaaaaa"
-- *Ch5> replica 10 1
-- [1,1,1,1,1,1,1,1,1,1]

-- A triple *(x, y, z)* of positive integers is pythagorean if *x^2 + y^2 = z^2*.
-- Using a list comprehension, define a function *pyths :: Int → [(Int, Int, Int)]* that
-- returns the list of all pythagorean triples whose components are at most a given limit.

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- m,
                       y <- m,
                       z <- m,
                       x^2 + y^2 == z^2 ]
          where m = [1..n]

-- *Ch5> pyths 10
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
