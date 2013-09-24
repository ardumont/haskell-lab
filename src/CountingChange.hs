module CountingChange where

import Data.List

countchange :: Int -> [Int] -> Int
countchange m c = countChange m $ (reverse . sort) c

countChange :: Int -> [Int] -> Int
countChange _ []       = 0
countChange 0 _        = 1
countChange m c@(x:xs) = if m < 0
                         then 0
                         else countChange m xs + countChange (m - x) c

-- *CountingChange> countchange 300 [5,10,20,50,100,200,500] -- 1022
-- 1022
-- *CountingChange> countchange 301 [5,10,20,50,100,200,500] -- 1022
-- 0
-- *CountingChange> countchange 4 [5,10,20,50,100,200,500] -- 1022
-- 0
-- *CountingChange> countchange 4 [1,2] -- 1022
-- 3
-- *CountingChange> countchange 4 [] -- 1022
-- 0
-- *CountingChange> countchange 0 [1,2] -- 1022
-- 1
-- *CountingChange> countchange 100 [50,25,10,5,1]
-- 292
