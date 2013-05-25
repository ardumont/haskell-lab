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

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ x y) = values x ++ values y

-- *Problem> values $ App Add (Val 10) (Val 20)
-- [10,20]
-- *Problem> values $ Val 10
-- [10]
-- *Problem> values $ App Add (App Sub (Val 10) (Val 3)) (App Div (Val 4) (Val 2))
-- [10,3,4,2]

eval :: Expr -> [Int]
eval (Val n) = [n | n>0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- *Problem> eval (App Add (App Sub (Val 10) (Val 3)) (App Div (Val 4) (Val 2)))
-- [9]
-- *Problem> eval (App Add (App Mul (Val 10) (Val 3)) (App Div (Val 4) (Val 2)))
-- [32]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = xxs ++ map (x:) xxs where xxs = subs xs

-- *Problem> subs [1,2]
-- [[],[2],[1],[1,2]]
-- *Problem> subs [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
-- *Problem> subs [1,2,3,4]
-- [[],[4],[3],[3,4],[2],[2,4],[2,3],[2,3,4],[1],[1,4],[1,3],[1,3,4],[1,2],[1,2,4],[1,2,3],[1,2,3,4]]
