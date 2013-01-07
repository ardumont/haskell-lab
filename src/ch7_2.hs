module Ch7_2 where

-- given a function that takes pair as parameter
add :: Num a => (a,a) -> a
add (x,y) = x + y

-- Given the curry function
cur :: ((a,b) -> c) -> a -> b -> c
cur f = \x -> \y -> f (x,y)

-- here are examples:
-- *Ch7_2> (cur add) 1 2
-- 3
-- *Ch7_2> ((cur add) 1) 2
-- 3

-- which can be put simply
currify :: ((a,b) -> c) -> a -> b -> c
currify f x y = f (x,y)

-- *Ch7_2> (currify add) 1 2
-- 3
-- *Ch7_2> ((currify add) 1) 2
-- 3

uncur :: (a -> b -> c) -> (a, b) -> c
uncur f = \ (x, y) -> (f x y)

-- *Ch7_2> add (1,2)
-- 3
-- *Ch7_2> (cur add) 1 2
-- 3
-- *Ch7_2> uncur (cur add) (1,2)
-- 3

-- which can be put simply
uncurrify :: (a -> b -> c) -> (a, b) -> c
uncurrify f (x, y) = f x y

-- *Ch7_2> uncurrify (currify add) (1,2)
-- 3
