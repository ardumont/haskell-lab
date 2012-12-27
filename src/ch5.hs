module Ch5 where

sq :: Num a => [a] -> [a]
sq g = [x^2 | x <- g]
