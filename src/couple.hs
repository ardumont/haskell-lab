module Couple where
import Data.Char
import Test.QuickCheck

-- unique couple (a,b) such that a*b = n
-- As the multiplication is commutative, we consider (a,b) == (b,a)

couple :: Int -> [(Int, Int)]
couple n = [(a,b) | a <- [2..n], b <- [1..a], a * b == n]

-- *Couple> quickCheck (\ x -> all (\ (a,b) -> b <= a) (couple x))
--   C-c C-c*** Failed! Exception: 'user interrupt' (after 27 tests):
-- 47540
-- took too long
