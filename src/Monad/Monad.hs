module Monad where

import Control.Monad.Writer

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- *Monad> fst $ runWriter (gcd' 21 8)
-- 1
-- *Monad> snd $ runWriter (gcd' 21 8)
-- ["21 mod 8 = 5","8 mod 5 = 3","5 mod 3 = 2","3 mod 2 = 1","2 mod 1 = 0","Finished with 1"]

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["gonna multiple this too"]
    return (a*b)

-- *Monad> runWriter multWithLog
-- (15,["Got number: 3","Got number: 5","gonna multiple this too"])
