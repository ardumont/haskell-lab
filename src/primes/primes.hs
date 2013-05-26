module Prime where

import Test.QuickCheck
import Data.Set (difference, toList, fromList)
import System.Environment

isPrime :: Integral a => a -> Bool
isPrime n
  | n <= 1 = False
  | n == 2 = True
  | otherwise =
  all ( \ y -> n `mod` y /= 0) pseudoPrimes
  where
    pseudoPrimes = (2:[3,5..(floor . sqrt . fromIntegral) n])

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:ps) = p : sieve [n | n <- ps, n `mod` p /= 0]

prop_prime :: Int -> Bool
prop_prime = \ n -> all isPrime (take n primes)

prop_not_prime :: Int -> Bool
prop_not_prime =
  \ n -> let noPrimes = toList $ difference (fromList [1..n]) (fromList (take n primes)) in
    all (not . isPrime) noPrimes

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
            putStrLn $ show (take n primes)
