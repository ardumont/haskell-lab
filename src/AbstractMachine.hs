module AbstractMachine where

data Expr = Val Int | Add Expr Expr | Mult Expr Expr deriving Show

value:: Expr -> Int
value (Val n)   = n
value (Mult x y) = value x + value y
value (Add x y) = value x + value y

-- *AbstractMachine> value (Mult (Val 10) (Val 5))
-- 15
-- *AbstractMachine> value (Add (Val 10) (Mult (Val 10) (Val 5)))
-- 25

data Op = EVALM Expr | EVALA Expr | MUL Int | ADD Int deriving Show

type Cont = [Op]

eval :: Expr -> Cont -> Int
eval (Val n)    c = exec c n
eval (Mult x y) c = eval x (EVALM y : c)
eval (Add x y)  c = eval x (EVALA y : c)

exec :: Cont -> Int -> Int
exec []            n = n
exec (EVALM y : c) n = eval y (MUL n : c)
exec (EVALA y : c) n = eval y (ADD n : c)
exec (MUL m : c)   n = exec c (n * m)
exec (ADD m : c)   n = exec c (n + m)

val :: Expr -> Int
val e = eval e []

-- *AbstractMachine> val (Mult (Val 10) (Val 5))
-- 50
-- *AbstractMachine> val (Add (Mult (Val 10) (Val 5)) (Val 100))
-- 150
