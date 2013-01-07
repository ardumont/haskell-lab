module Ch7_2 where

-- given a function that takes pair as parameter
add :: Num a => (a,a) -> a
add (x,y) = x + y

-- given the curry function
cur :: ((a,b) -> c) -> a -> b -> c
cur f = \x -> \y -> f (x,y)

-- here are examples:
-- *Ch7_2> ((cur add) 1) 2
-- 3
-- *Ch7_2> (cur add) 1 2
-- 3
