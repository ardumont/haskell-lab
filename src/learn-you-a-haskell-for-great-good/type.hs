module Type where

removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase cs = [ c | c <- cs, c `elem` ['A'..'Z'] ]

-- *Type> removeNonUpperCase "abcdEFG"
-- "EFG"

-- Int     - bounded integers 2^32
-- Integer - boundless integers

factorial :: Integer -> Integer
factorial n = product [1..n]

-- *Type> factorial 50
-- 30414093201713378043612608166064768844377641568960512000000000000

-- *Type> :t pi
-- pi :: Floating a => a
-- *Type> pi
-- 3.141592653589793

circumference :: Float -> Float
circumference r = 2 * pi * r

-- *Type> circumference 2
-- 12.566371

-- function with type a are called polymorphic functions
