module Primes.Prime where

import           Data.Set           (difference, fromList, toList)
import           System.Environment
import           Test.QuickCheck

isPrime :: Integral a => a -> Bool
isPrime n
  | n <= 1 = False
  | n == 2 = True
  | otherwise =
  all ( \ y -> n `mod` y /= 0) pseudoPrimes
  where
    pseudoPrimes = 2:[3,5..(floor . sqrt . fromIntegral) n]

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:ps) = p : sieve [n | n <- ps, n `mod` p /= 0]

-- *Prime> filter isPrime [1..100] == take 25 primes
-- True

prop_prime :: Property
prop_prime = forAll
            (elements [1..10000])
            (\ num -> let n = abs num in all isPrime (takeWhile (< n) primes))

prop_not_prime :: Property
prop_not_prime =
  forAll
  (elements [1..10000])
  (\ n -> let ps = takeWhile (< n) primes
              noPrimes = toList $ difference (fromList [1..n]) (fromList ps) in
    all (not . isPrime) noPrimes)

deepCheck :: Testable prop => prop -> IO ()
deepCheck = quickCheckWith stdArgs { maxSuccess = 500}

test :: IO ()
test = do
  -- verboseCheckWith stdArgs { maxSuccess = 10000 } prop_prime
  -- verboseCheckWith stdArgs { maxSuccess = 10000 } prop_not_prime
  deepCheck prop_prime
  deepCheck prop_not_prime

-- *Sieve> test
-- +++ OK, passed 500 tests.
-- +++ OK, passed 500 tests.

-- Now we can make it as a script
main :: IO ()
main = do
  (num:_) <- getArgs
  let n = read num :: Int in
    print $ show (take n primes)
