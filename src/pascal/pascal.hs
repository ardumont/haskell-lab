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
triangleRow 0 = [1]
triangleRow 1 = [1,1]
triangleRow n = tRow p (triangleRow p)
                where p = n-1
                      tRow :: Int -> [Int] -> [Int]
                      tRow 0 r = r
                      tRow m r = tRow (m-1) (triangle r)

-- *Pascal> triangleRow 0
-- [1]
-- *Pascal> triangleRow 1
-- [1,1]
-- *Pascal> triangleRow 2
-- [1,2,1]
-- *Pascal> triangleRow 3
-- [1,4,6,4,1]
-- *Pascal> triangleRow 4
-- [1,7,21,35,35,21,7,1]
-- *Pascal> triangleRow 5
-- [1,11,55,165,330,462,462,330,165,55,11,1]
-- *Pascal> triangleRow 6
-- [1,16,120,560,1820,4368,8008,11440,12870,11440,8008,4368,1820,560,120,16,1]

get :: Int -> Int -> Int
get c r = (last . take col . triangleRow) r
          where col = 1+ c

-- *Pascal> get 3 3
-- 4
-- *Pascal> pprint 4
-- [1]
-- [1,1]
-- [1,2,1]
-- [1,4,6,4,1]
-- [1,7,21,35,35,21,7,1]
-- *Pascal> get 3 4
-- 35
-- *Pascal> get 3 4
-- 35
-- *Pascal> get 4 4
-- 35
-- *Pascal> get 5 4
-- 21


triangleRows :: Int -> [[Int]]
triangleRows n = map triangleRow [0..n]

pprint :: Int -> IO ()
pprint n = mapM_ print (triangleRows n)

-- *Pascal> pprint 3
-- [1]
-- [1,1]
-- [1,2,1]
-- [1,4,6,4,1]
-- *Pascal> pprint 4
-- [1]
-- [1,1]
-- [1,2,1]
-- [1,4,6,4,1]
-- [1,7,21,35,35,21,7,1]
-- *Pascal> pprint 5
-- [1]
-- [1,1]
-- [1,2,1]
-- [1,4,6,4,1]
-- [1,7,21,35,35,21,7,1]
-- [1,11,55,165,330,462,462,330,165,55,11,1]
-- *Pascal> pprint 6
-- [1]
-- [1,1]
-- [1,2,1]
-- [1,4,6,4,1]
-- [1,7,21,35,35,21,7,1]
-- [1,11,55,165,330,462,462,330,165,55,11,1]
-- [1,16,120,560,1820,4368,8008,11440,12870,11440,8008,4368,1820,560,120,16,1]
-- *Pascal> pprint 7
-- [1]
-- [1,1]
-- [1,2,1]
-- [1,4,6,4,1]
-- [1,7,21,35,35,21,7,1]
-- [1,11,55,165,330,462,462,330,165,55,11,1]
-- [1,16,120,560,1820,4368,8008,11440,12870,11440,8008,4368,1820,560,120,16,1]
-- [1,22,231,1540,7315,26334,74613,170544,319770,497420,646646,705432,646646,497420,319770,170544,74613,26334,7315,1540,231,22,1]
