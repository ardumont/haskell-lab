module Ch3 where

power :: (Integral a1, Num a) => a -> a1 -> a
power x k = if k == 0
            then 1
            else if (k `mod` 2) == 0
                 then power (x*x) (k `div` 2)
                 else x * power (x*x) (k `div` 2)

prodsum :: (Eq a, Num a) => a -> a
prodsum x = prod x + s x

prod :: (Eq a, Num a) => a -> a
prod 0 = 1
prod n = n * prod (n-1)

s :: (Eq a, Num a) => a -> a
s 0 = 0
s n = n + s (n-1)
