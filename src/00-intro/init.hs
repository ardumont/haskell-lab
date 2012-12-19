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
rqsort xs = reverse (qsort xs)

-- rqsort [3, 1, 7, 9, 6]
-- [9,7,6,3,1]
