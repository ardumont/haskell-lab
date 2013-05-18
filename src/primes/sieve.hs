module Sieve where

prime :: Integral a => a -> [a]
prime l =
  sieve [2..l] []
  where
    sieve []     r = r
    sieve (x:xs) r =
      sieve (filter ( (/= 0) . flip (mod) x) xs) (x:r)
