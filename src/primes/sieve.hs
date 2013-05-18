module Sieve where

prime :: Integral a => a -> [a]
prime r =
  sieve i []
  where
    i                  = [2..r]
    sieve []     range = range
    sieve (x:xs) range =
      sieve (filter (\ n -> n `mod` x /= 0) xs) (x:range)
