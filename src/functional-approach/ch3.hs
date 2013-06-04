module Ch3 where

power :: (Integral a1, Num a) => a -> a1 -> a
power x k = if k == 0
            then 1
            else if (k `mod` 2) == 0
                 then power (x*x) (k `div` 2)
                 else x * power (x*x) (k `div` 2)
