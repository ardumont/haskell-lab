module Sieve where

import Test.QuickCheck

isPrime :: Integral a => a -> Bool
isPrime n
  | n <= 1 = False
  | n == 2 = True
  | otherwise =
  all ( \ y -> n `mod` y /= 0) pseudoPrimes
  where
    pseudoPrimes = (2:[3,5..(floor . sqrt . fromIntegral) n])

prime :: Integral a => a -> [a]
prime l =
  sieve [2..l] []
  where
    sieve []     r = r
    sieve (x:xs) r =
      sieve (filter ( (/= 0) . flip (mod) x) xs) (x:r)

-- *Sieve> filter isPrime [1..100] == (reverse . prime) 100
-- True

prop_prime :: Integer -> Bool
prop_prime = (\ n -> all isPrime (prime n))

-- -- -- adding
main :: IO ()
main = do
  -- verboseCheckWith stdArgs { maxSuccess = 1000, maxSize = 5 } prop_prime
  verboseCheckWith stdArgs { maxSuccess = 10000 } prop_prime
  -- verboseCheckWith stdArgs prop_prime

-- Passed:
-- -43
-- Passed:
-- -23
-- Passed:
-- 20
-- Passed:
-- -21
-- Passed:
-- -36
-- Passed:
-- -18
-- Passed:
-- -75
-- +++ OK, passed 10000 tests.
