module Sieve where

import Test.QuickCheck
import Data.Set
import System.Environment

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
      sieve (Prelude.filter ( (/= 0) . flip (mod) x) xs) (x:r)

-- *Sieve> filter isPrime [1..100] == (reverse . prime) 100
-- True

prop_prime :: Integer -> Bool
prop_prime = (\ n -> all isPrime (prime n))

prop_not_prime :: Integer -> Bool
prop_not_prime =
  (\ n -> let noPrimes = toList $ difference (fromList [1..n]) (fromList (prime n)) in
    all (not . isPrime) noPrimes)

deepCheck :: Testable prop => prop -> IO ()
deepCheck p = quickCheckWith stdArgs { maxSuccess = 10000} p

test :: IO ()
test = do
  -- verboseCheckWith stdArgs { maxSuccess = 10000 } prop_prime
  -- verboseCheckWith stdArgs { maxSuccess = 10000 } prop_not_prime
  deepCheck prop_prime
  deepCheck prop_not_prime

-- *Sieve> test
-- +++ OK, passed 10000 tests.
-- +++ OK, passed 10000 tests.

-- Now we can make it as a script
main :: IO ()
main = do (num:_) <- getArgs
          let n = read num :: Int in
            putStrLn $ (show . prime) n
