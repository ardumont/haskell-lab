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
