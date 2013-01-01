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

-- *Ch6> ins 10 [2,3,40]
-- [2,3,10,40]
-- *Ch6> ins 1 [2,3,40]
-- [1,2,3,40]

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = ins x (isort xs)

-- *Ch6> isort [43,29,10]
-- [10,29,43]

zp :: [a] -> [a] -> [(a,a)]
zp [] _ = []
zp _ [] = []
zp (x:xs) (y:ys) = (x,y):(zip xs ys)

-- *Ch6> zp [1,2,3,4] [5..10]
-- [(1,5),(2,6),(3,7),(4,8)]

drp :: Int -> [a] -> [a]
drp 0 ys = ys
drp _ [] = []
drp n (_:ys) = drp (n-1) ys

-- *Ch6> drp 10 [10..25]
-- [20,21,22,23,24,25]

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

ini :: [a] -> [a]
ini [_] = []
ini (x:xs) = x:(ini xs)

-- *Ch6> [1..10]
-- [1,2,3,4,5,6,7,8,9,10]
-- *Ch6> ini [1..10]
-- [1,2,3,4,5,6,7,8,9]

pow :: Int -> Int -> Int
pow _ 0 = 1
pow x n = x * pow x (n-1)

(^) :: Int -> Int -> Int
_ ^ 0 = 1
x ^ n = x * (x Ch6.^ (n-1))

-- *Ch6> 2 Ch6.^ 10
-- 1024

-- Decide if all logical values in a list are True: =and :: [Bool] → Bool=
every :: [Bool] -> Bool
every [] = True
every (x:xs) = x && (every xs)

-- *Ch6> every [True, True, True]
-- True
-- *Ch6> every [True, False, True]
-- False

-- Concatenate a list of lists: =concat :: [[a] ] → [a]=
ccat :: [[a]] -> [a]
ccat [xs] = xs
ccat (xs:xxs) = xs ++ (ccat xxs)

-- *Ch6> ccat [[1,2,3], [4,5,6], [10..20]]
-- [1,2,3,4,5,6,10,11,12,13,14,15,16,17,18,19,20]

-- Produce a list with n identical elements: =replic\ate :: Int → a → [a]
repli :: Int -> a -> [a]
repli 0 _ = []
repli n x = x:(repli (n-1) x)

-- *Ch6> repli 5 9
-- [9,9,9,9,9]
-- *Ch6> repli 10 'x'
-- "xxxxxxxxxx"

-- Select the n^th element of a list: =(!!) :: [a] → Int → a=
(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs Ch6.!! (n-1)

-- *Ch6> [1,2,3] Ch6.!! 2
-- 3
-- *Ch6> [1,2,3] Ch6.!! 0
-- 1
