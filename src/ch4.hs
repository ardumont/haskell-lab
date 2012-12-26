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
