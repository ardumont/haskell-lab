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

triangleRow :: Int -> [Int]
triangleRow n = foldr (\ _ ns -> triangle ns) [1] [1..n]

-- *Pascal> triangleRow 0
-- [1]
-- *Pascal> triangleRow 1
-- [1,1]
-- *Pascal> triangleRow 2
-- [1,2,1]
-- *Pascal> triangleRow 3
-- [1,3,3,1]
-- *Pascal> triangleRow 4
-- [1,4,6,4,1]
-- *Pascal> triangleRow 5
-- [1,5,10,10,5,1]

get :: Int -> Int -> Int
get c r = (last . take col . triangleRow) r
          where col = 1+ c

-- *Pascal> get 3 4
-- 4
-- *Pascal> get 4 4
-- 1
-- *Pascal> get 1 3
-- 3

triangleRows :: Int -> [[Int]]
triangleRows n = map triangleRow [0..n]

pprint :: Int -> IO ()
pprint n = mapM_ print (triangleRows n)

-- *Pascal> pprint 10
-- [1]
-- [1,1]
-- [1,2,1]
-- [1,3,3,1]
-- [1,4,6,4,1]
-- [1,5,10,10,5,1]
-- [1,6,15,20,15,6,1]
-- [1,7,21,35,35,21,7,1]
-- [1,8,28,56,70,56,28,8,1]
-- [1,9,36,84,126,126,84,36,9,1]
-- [1,10,45,120,210,252,210,120,45,10,1]
