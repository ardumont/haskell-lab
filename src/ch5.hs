module Ch5 where

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
