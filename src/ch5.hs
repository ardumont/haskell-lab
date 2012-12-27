module Ch5 where

-- generators

sq :: Num a => [a] -> [a]
sq g = [x^2 | x <- g]

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

lgth :: Num b => [a] -> b
lgth xs = sum [1 | _ <-xs]
