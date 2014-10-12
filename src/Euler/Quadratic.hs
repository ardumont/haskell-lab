{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Euler.Quadratic where

import           Data.Function (on)
import           Data.List     (intersect, maximumBy, sortBy)
import           Primes.Prime  (isPrime)

-- 27 - Quadratic prime
-- Euler discovered the remarkable quadratic formula:
-- n² + n + 41
-- It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.
-- The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.
-- Considering quadratics of the form:
--     n² + an + b, where |a| < 1000 and |b| < 1000
--     where |n| is the modulus/absolute value of n
--     e.g. |11| = 11 and |−4| = 4
-- Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

-- | Compute the sequence of primes for a given pair of coefficients
computePrime :: Integral a => (a, a) -> [a]
computePrime (a, b) = takeWhile isPrime [ n^2 + a * n + b | n <- [0..]]

-- | Compute the possible coefficients
coefficients :: (Enum t, Integral t1, Num t) => [(t, t1)]
coefficients = [ (a, b) | a <- [-999..999]
                        , b <- filter isPrime [0..999]]

-- | Compute the product of coefficients for which the number of primes generated is greater
computeProduct :: Integral a => a
computeProduct = fst $ maximumBy (compare `on` snd) $ map (\ c@(a, b) -> (a * b, length $ computePrime c)) $ coefficients

-- λ> computeProduct
-- -59231
