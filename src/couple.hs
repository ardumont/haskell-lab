module Couple where
import Data.Char
import Test.QuickCheck
import Test.QuickCheck.All

-- unique couple (a,b) such that a*b = n
-- As the multiplication is commutative, we consider (a,b) == (b,a)

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

couple :: Int -> [(Int, Int)]
couple n = [(a,b) | a <- [1..isqrt n], b <- [a..n], a * b == n]

coupleSimple :: Int -> [(Int, Int)]
coupleSimple n = [(a,b) | a <- [1..n], b <- [a..n], a * b == n]

-- *Couple> couple 10
-- [(1,10),(2,5)]
-- *Couple> couple 12
-- [(1,12),(2,6),(3,4)]
-- *Couple> couple 24
-- [(1,24),(2,12),(3,8),(4,6)]
-- *Couple> couple 100
-- [(1,100),(2,50),(4,25),(5,20),(10,10)]
-- *Couple> couple 1000
-- [(1,1000),(2,500),(4,250),(5,200),(8,125),(10,100),(20,50),(25,40)]
-- *Couple> couple 1000
-- [(1,1000),(2,500),(4,250),(5,200),(8,125),(10,100),(20,50),(25,40)]
-- *Couple> couple 10000
-- [(1,10000),(2,5000),(4,2500),(5,2000),(8,1250),(10,1000),(16,625),(20,500),(25,400),(40,250),(50,200),(80,125),(100,100)]

rg :: Int -> Int -> [a] -> [a]
rg inf sup s = take sup $ drop inf s

rgc :: Int -> Int -> [(Int, Int, [(Int, Int)])]
rgc inf sup = rg inf sup [(n, isqrt n, couple n) | n <- [1..]]

-- *Couple> quickCheck (\ x -> all (\ (a,b) -> b <= a) (couple x))
--   C-c C-c*** Failed! Exception: 'user interrupt' (after 27 tests):
-- 47540
-- took too long

-- test
-- property: all a is superior or equal to b
-- checkOnly10 p = Q.check (Q.defaultConfig { configMaxTest = 10}) p
-- verboseCheck (\ n -> all (\ (a,b) -> ( ((a == b) || (a, b) /= (b, a)) && b <= a && a * b == n) ) (couple n))

-- define the properties to check
prop_productOk = (\ n -> all (\ (a,b) -> a * b == n ) (couple n))
prop_coupleIdempotence = (\ x y -> couple x == couple y)
prop_coupleInfSqrt = (\ n -> all (\ (a,b) -> a <= isqrt n ) (couple n))
prop_coupleVsCoupleSimple = (\ n -> (couple n) == (coupleSimple n))

-- adding
main = do
  verboseCheckWith stdArgs { maxSuccess = 1000, maxSize = 5 } prop_productOk
  verboseCheckWith stdArgs { maxSuccess = 1000, maxSize = 5 } prop_coupleIdempotence
  verboseCheckWith stdArgs { maxSuccess = 1000, maxSize = 5 } prop_coupleInfSqrt
  verboseCheckWith stdArgs { maxSuccess = 1000, maxSize = 5 } prop_coupleVsCoupleSimple
