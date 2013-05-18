module Sieve where

prime :: Integral a => a -> [a]
prime l =
  sieve [2..l] []
  where
    sieve []     r = r
    sieve (x:xs) r =
      sieve (filter (\ n -> n `mod` x /= 0) xs) (x:r)
