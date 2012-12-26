module Ch4 where
isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

evn :: Integral a => a -> Bool
evn x = x `mod` 2 == 0

spltAt :: Int -> [a] -> ([a], [a])
spltAt i xs = (take i xs, drop i xs)

recipr :: Fractional a => a -> a
recipr n = 1 / n

abso1 :: Int -> Int
abso1 n = if n >= 0 then n else -n

-- abso1 (-10) -- 10
-- abso1 10 -- 10
signum1 :: Int -> Int
signum1 x = if x < 0 then -1
            else if x > 0 then 1 else 0

-- guarded equations

abso2 :: Int -> Int
abso2 n | n >= 0 = n
       | otherwise = -n

signum2 :: Int -> Int
signum2 x | x < 0 = -1
          | x > 0 = 1
          | otherwise = 0

-- pattern matching

nott :: Bool -> Bool
nott False = True
nott True  = False

andd :: Bool -> Bool -> Bool
andd True True = True
andd _ _       = False

-- Tuple patterns
fstt :: (a,a) -> a
fstt (a,_) = a

sndd :: (a,a) -> a
sndd (_,a) = a

-- list patterns

nil :: [a] -> Bool
nil [] = True
nil (_:_) = False

headd :: [a] -> a
headd (x:_) = x

taill :: [a] -> [a]
taill (_:xs) = xs

-- lambda expressions

odds :: Int -> [Int]
odds n = map (\ x -> 2 * x + 1) [0..n-1]

-- Exercises

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- safetail

safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs   = []
             | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs

-- logical conjunction

orr :: Bool -> Bool -> Bool
orr False False = False
orr True True   = True
orr True False  = True
orr False True  = True

orr2 :: Bool -> Bool -> Bool
orr2 False False = False
orr2 _     _     = True

orr3 :: Bool -> Bool -> Bool
orr3 False b     = b
orr3 b     False = b
orr3 _     _     = True

orr4 :: Bool -> Bool -> Bool
orr4 False b = b
orr4 True  _ = True

-- orr4 :: Bool -> Bool -> Bool
-- orr4 x x = x
-- orr4 _ _ = True
-- Do not get why this poses pb

-- map (\ (f,s) -> orr4 f s) [(False, False), (False, True), (True, False), (True, True)]

-- conjunction operator

and1 :: Bool -> Bool -> Bool
and1 a b = if not a
           then False
           else if not b
                then False
                else True

and2 :: Bool -> Bool -> Bool
and2 a b | not a = False
         | not b = False
         | otherwise = True
-- map (\ (f,s) -> and1 f s) [(False, False), (False, True), (True, False), (True, True)]
-- [False, False, False, True]
