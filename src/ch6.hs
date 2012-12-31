module Ch6 where

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
-- factorial (n+1) = (n+1) * factorial n -- does not work, do not know why

-- (*) :: Int -> Int -> Int
-- _ * 0 = 0
-- m * n = m + m Ch6.* (n - 1)
--m * (n + 1) = m + m * n -- does not work, do not know why

pdt :: [Int] -> Int
pdt [] = 1
pdt (x:xs) = x * pdt xs

-- *Ch6> pdt [1..10]
-- 3628800

lgth :: [a] -> Int
lgth [] = 0
lgth (_:xs) = 1 + lgth xs

-- *Ch6> lgth [1..10]
-- 10

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = (rev xs) ++ [x]

-- *Ch6> rev [1,3,4]
-- [4,3,1]

-- (++) :: [a] -> [a] -> [a]
-- [] ++ ys = ys
-- (x:xs) ++ ys = x:(xs Ch6.++ ys)

-- *Ch6> [1..5] Ch6.++ [2..4]
-- [1,2,3,4,5,2,3,4]
