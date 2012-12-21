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

-- take
-- take 2 [1,3,4,5,8]
-- [1,3]

pdt :: Num a => [a] -> a
pdt [] = 1
pdt (x:xs) = x * pdt xs

-- pdt [2,3,4]
-- 24

uqsort :: Ord a => [a] -> [a]
uqsort [] = []
uqsort (x:xs) = rqsort (filter (< x) xs) ++ [x] ++ rqsort (filter (> x) xs)
