module Ch1 where

fact :: Int -> Int
fact n
  | n < 0     = -1
  | n == 0    = 1
  | otherwise = n * fact (n-1)

-- *Ch1> fact 0
-- 1
-- *Ch1> fact 1
-- 1
-- *Ch1> fact 5
-- 120
-- *Ch1> fact (-10)
-- -1

f :: [Int] -> [Int]
f l = reverse (f' l [])
      where f' [] r     = r
            f' (x:xs) r = (2*x) : (f' xs r)

avg :: [Int] -> Int
avg xs | null xs   = 0
       | otherwise = (sum xs) `div` (length xs)

-- *Ch1> avg [1..10]
-- 5
-- *Ch1> avg []
-- 0

mdl :: [a] -> Maybe a
mdl xs | null xs   = Nothing
       | otherwise = Just (xs !! p)
                     where p = ((subtract 1) . (`div` 2) . length) xs

-- *Ch1> mdl [1..10]
-- Just 5
-- *Ch1> mdl [1..20]
-- Just 10
-- *Ch1> mdl []
-- Nothing
