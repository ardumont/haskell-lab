module Problem where

-- operation
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

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- *Problem> apply Add 10 20
-- 30
-- *Problem> apply Add 20 10
-- 30
-- *Problem> apply Sub 20 10
-- 10
-- *Problem> apply Mul 20 10
-- 200
-- *Problem> apply Div 20 10
-- 2

-- expression
data Expr = Val Int | App Op Expr Expr

-- *Problem> :t Val 10
-- Val 10 :: Expr
-- *Problem> :t App Add (Val 10) (Val 20)
-- App Add (Val 10) (Val 20) :: Expr
