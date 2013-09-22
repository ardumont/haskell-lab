module Pascal where

triangle :: [Int] -> [Int]
triangle xs = tri xs [1]
              where tri :: [Int] -> [Int] -> [Int]
                    tri [x] r      = x:r
                    tri (x:y:ys) r = tri (y:ys) $ (x+y):r

-- *Pascal> triangle [1]
-- [1,1]
-- *Pascal> triangle [1,1]
-- [1,2,1]
-- *Pascal> triangle [1,2,1]
-- [1,3,3,1]
-- *Pascal> triangle [1,3,3,1]
-- [1,4,6,4,1]
-- *Pascal> triangle [1,4,6,4,1]
-- [1,5,10,10,5,1]
-- *Pascal> triangle [1,5,10,10,5,1]
-- [1,6,15,20,15,6,1]
-- *Pascal> triangle [1,6,15,20,15,6,1]
-- [1,7,21,35,35,21,7,1]
