module Ch3 where

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,a) -> (a,a)
swap (x , y) = (y, x)

pair :: a -> a -> (a, a)
pair x y = (x , y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)
