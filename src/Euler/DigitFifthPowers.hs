-- | Compute the sum of numbers to which their digits elevated to some exponent is themselves.

module Euler.DigitFifthPowers where

-- https://projecteuler.net/problem=30
-- Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

--     1634 = 14 + 64 + 34 + 44
--     8208 = 84 + 24 + 04 + 84
--     9474 = 94 + 44 + 74 + 44

-- As 1 = 14 is not a sum it is not included.

-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.

-- Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

import           Data.Digits (digits)

-- | Determine the set of integers to lookup for a specific exponent
setForExponent :: Integral b => b -> [b]
setForExponent e = [2..e * (^e) 9]

-- | Compute the sum of digits of n elevated to the exponent e
sumDigits :: (Integral b, Integral c) => b -> c -> c
sumDigits e = sum . map (^e) . digits 10

-- | Is the sum of digits of n elevated to the exponent e equal to n?
isSumDigitsItself :: Integral a => a -> a -> Bool
isSumDigitsItself e n = sumDigits e n == n

-- | Compute the sum of digits
computeSumDigits :: Integral a => a -> a
computeSumDigits e = sum [ x | x <- setForExponent e, isSumDigitsItself e x ]

-- λ> computeSumDigits 4
-- 19316
-- λ> computeSumDigits 5
-- 443839
