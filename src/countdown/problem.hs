module Problem where

data Op = Add | Sub | Mul | Div

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- *Problem> valid Add 10 20
-- True
-- *Problem> valid Sub 10 20
-- False
-- *Problem> valid Sub 20 10
-- True
-- *Problem> valid Mul 10 20
-- True
-- *Problem> valid Div 10 20
-- False
-- *Problem> valid Div 20 10
-- True
