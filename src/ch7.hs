module Ch7 where

fmap0 :: (a -> a) -> (a -> Bool) -> [a] -> [a]
fmap0 f p xs = [ f x | x <- xs, p x]

-- *Ch7> fmap0 (+1) even [1..10]
-- [3,5,7,9,11]

fmap1 :: (a -> a) -> (a -> Bool) -> [a] -> [a]
fmap1 f p xs = map f (filter p xs)

-- *Ch7> fmap1 (+1) even [1..10]
-- [3,5,7,9,11]
