module Couple where
import Data.Char
import Test.QuickCheck
import Test.QuickCheck.All

-- unique couple (a,b) such that a*b = n
-- As the multiplication is commutative, we consider (a,b) == (b,a)

couple :: Int -> [(Int, Int)]
couple n = [(a,b) | a <- [1..n], b <- [1..a], a * b == n]


-- *Couple> couple 10
-- [(5,2),(10,1)]
-- *Couple> couple 100
-- [(10,10),(20,5),(25,4),(50,2),(100,1)]
-- *Couple> couple 12
-- [(4,3),(6,2),(12,1)]
-- *Couple> couple 24
-- [(6,4),(8,3),(12,2),(24,1)]
-- *Couple> couple 1000
-- [(40,25),(50,20),(100,10),(125,8),(200,5),(250,4),(500,2),(1000,1)]
-- *Couple> couple 10000
-- [(100,100),(125,80),(200,50),(250,40),(400,25),(500,20),(625,16),(1000,10),(1250,8),(2000,5),(2500,4),(5000,2),(10000,1)]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

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

-- adding
main = do
  verboseCheckWith stdArgs { maxSuccess = 10 } prop_productOk
  verboseCheckWith stdArgs { maxSuccess = 10 } prop_coupleIdempotence
