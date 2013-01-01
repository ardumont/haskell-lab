module Ch6 where

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- (*) :: Int -> Int -> Int
-- _ * 0 = 0
-- m * n = m + m Ch6.* (n - 1)

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

ins :: Ord a => a -> [a] -> [a]
ins x [] = [x]
ins x (y:ys) | x <= y    = x:y:ys
             | otherwise = y:(ins x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = ins x (isort xs)

zp :: [a] -> [a] -> [(a,a)]
zp [] _ = []
zp _ [] = []
zp (x:xs) (y:ys) = (x,y):(zip xs ys)

drp :: Int -> [a] -> [a]
drp 0 ys = ys
drp _ [] = []
drp n (_:ys) = drp (n-1) ys

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

ini :: [a] -> [a]
ini [_] = []
ini (x:xs) = x:(ini xs)

pow :: Int -> Int -> Int
pow _ 0 = 1
pow x n = x * pow x (n-1)

every :: [Bool] -> Bool
every [] = True
every (x:xs) = x && (every xs)
