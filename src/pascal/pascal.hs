module Pascal where

triangle :: [Int] -> [Int]
triangle xs = tri xs [1]
              where tri :: [Int] -> [Int] -> [Int]
                    tri [x] r      = x:r
                    tri (x:y:ys) r = tri (y:ys) ((x+y):r)
