module Ch6 where

factorial :: Int -> Int
factorial 0 = 1
factorial n = n Prelude.* factorial (n - 1)
-- factorial (n+1) = (n+1) * factorial n -- does not work, do not know why

(*) :: Int -> Int -> Int
_ * 0 = 0
m * n = m + m Ch6.* (n - 1)
--m * (n + 1) = m + m * n -- does not work, do not know why
