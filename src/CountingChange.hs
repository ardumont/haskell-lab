module CountingChange where

import Data.List

countchange :: Int -> [Int] -> Int
countchange m c = countChange m $ (reverse . sort) c

countChange :: Int -> [Int] -> Int
countChange 0 _       = 1
countChange _ []      = 0
countChange m [x]     = countChange (m - x) [x]
countChange m c@(x:_) = countChange m [x] + countChange (m - x) c

-- 300 [100 20 10 5 1]
-- 100 + 100 + 100
-- 200 [100 20 10 5 1]

-- countchange 4 [1,2] -- 3
-- countchange 300 [5,10,20,50,100,200,500] -- 1022
-- countchange 301 [5,10,20,50,100,200,500] -- 0
