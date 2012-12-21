module Init where

doublee :: Num a => a -> a
doublee x = x + x

-- doublee 2
-- 4

-- doublee (doublee (doublee 2))
-- 16

-- sum
summ :: Num a => [a] -> a
summ [] = 0
summ (x:xs) = x + summ xs

-- summ [1,2,3,4]
-- 4
-- summ [1..10]
-- 55

-- qsort
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (filter (<= x) xs) ++ [x] ++ qsort (filter (> x) xs)

-- qsort [3, 1, 7, 9, 6]
-- [1,3,6,7,9]

rqsort :: Ord a => [a] -> [a]
rqsort [] = []
rqsort (x:xs) = rqsort (filter (>= x) xs) ++ [x] ++ rqsort (filter (< x) xs)

-- rqsort [3, 1, 7, 9, 6]
-- [9,7,6,3,1]

pdt :: Num a => [a] -> a
pdt [] = 1
pdt (x:xs) = x * pdt xs

-- pdt [2,3,4]
-- 24

uqsort :: Ord a => [a] -> [a]
uqsort [] = []
uqsort (x:xs) = rqsort (filter (< x) xs) ++ [x] ++ rqsort (filter (> x) xs)

-- head [3,4,5]
-- 3

-- head [1]
-- 1

-- head []
-- error

-- tail [3,4,5]
-- [4.5]

-- tail [1]
-- []

-- tail []
-- error

-- [1,2,3,98,99] !! 3
-- 98

-- take 2 [1,3,4,5,8]
-- [1,3]

-- drop 2 [1,3,4,5,8]
-- [4,5,8]

-- length [1,3,5,6]
-- 4

-- sum [1,3,5,6]
-- 15

-- product [1,3,5,6]
-- 90

-- [1,2,3] ++ [4,5]
-- [1,2,3,4,5]

-- reverse [1,2,3,4,5]
-- [5,4,3,2,1]

-- n = a `div` (length xs)
--     where
--       a  = 10
--       xs = [1, 2, 3, 4, 5]

-- function `currify` that curries any function with 2 variables
currify :: ((a,a) -> a) -> (a -> a -> a)
currify f x y = f(x,y)

multiply :: Num a => (a,a) -> a
multiply (x, y) = x * y

-- multiply10 = (currify multiply) 10

-- function uncurrify
uncurrify :: (a -> a -> a) -> (a,a) -> a
uncurrify f (x,y) = f x y

-- map
-- map (\ x  -> 2  * x) [1, 3, 4]
-- [2,6,8]
