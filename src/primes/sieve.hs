module Sieve where

prime :: Integral a => [a] -> [a]
prime r = sieve [2] r
          where
            sieve []  r = r
            sieve [x] r =
              let l = filter (\ n -> n `mod` x /= 0) r in
              if (null l) || (length l) == (length r)
              then r
              else sieve [(head l)] $ x : (tail l)
