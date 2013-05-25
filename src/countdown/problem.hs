module Problem where

-- operation
data Op = Add | Sub | Mul | Div deriving (Show)

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
data Expr = Val Int | App Op Expr Expr deriving (Show)

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
subs []     = [[]]
subs (x:xs) = xxs ++ map (x:) xxs where xxs = subs xs

-- *Problem> subs [1,2]
-- [[],[2],[1],[1,2]]
-- *Problem> subs [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
-- *Problem> subs [1,2,3,4]
-- [[],[4],[3],[3,4],[2],[2,4],[2,3],[2,3,4],[1],[1,4],[1,3],[1,3,4],[1,2],[1,2,4],[1,2,3],[1,2,3,4]]

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- *Problem> interleave 10 [2,3]
-- [[10,2,3],[2,10,3],[2,3,10]]
-- *Problem> interleave 1 [2,3]
-- [[1,2,3],[2,1,3],[2,3,1]]
-- *Problem> interleave 4 [1,2,3]
-- [[4,1,2,3],[1,4,2,3],[1,2,4,3],[1,2,3,4]]

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concatMap (interleave x) . perms $ xs

-- *Problem> perms [1,2,3]
-- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-- *Problem> perms [1..4]
-- [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1],[1,3,2,4],[3,1,2,4],[3,2,1,4],[3,2,4,1],[1,3,4,2],[3,1,4,2],[3,4,1,2],[3,4,2,1],[1,2,4,3],[2,1,4,3],[2,4,1,3],[2,4,3,1],[1,4,2,3],[4,1,2,3],[4,2,1,3],[4,2,3,1],[1,4,3,2],[4,1,3,2],[4,3,1,2],[4,3,2,1]]

choices :: [a] -> [[a]]
choices = (concatMap perms) . subs

-- *Problem> choices [1,2,3]
-- [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-- *Problem> choices [1,2,3,4]
-- [[],[4],[3],[3,4],[4,3],[2],[2,4],[4,2],[2,3],[3,2],[2,3,4],[3,2,4],[3,4,2],[2,4,3],[4,2,3],[4,3,2],[1],[1,4],[4,1],[1,3],[3,1],[1,3,4],[3,1,4],[3,4,1],[1,4,3],[4,1,3],[4,3,1],[1,2],[2,1],[1,2,4],[2,1,4],[2,4,1],[1,4,2],[4,1,2],[4,2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1],[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1],[1,3,2,4],[3,1,2,4],[3,2,1,4],[3,2,4,1],[1,3,4,2],[3,1,4,2],[3,4,1,2],[3,4,2,1],[1,2,4,3],[2,1,4,3],[2,4,1,3],[2,4,3,1],[1,4,2,3],[4,1,2,3],[4,2,1,3],[4,2,3,1],[1,4,3,2],[4,1,3,2],[4,3,1,2],[4,3,2,1]]

ex :: Expr
ex = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))
-- *Problem> eval e
-- [765]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- *Problem> solution ex [1,3,7,10,25,50] 765
-- True

-- all possible ways of splitting a list that when concatenating gives the original list
split :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x: ls, rs) | (ls, rs) <- split xs]

-- *Problem> split [1,3,7,10,25,50]
-- [([1],[3,7,10,25,50]),([1,3],[7,10,25,50]),([1,3,7],[10,25,50]),([1,3,7,10],[25,50]),([1,3,7,10,25],[50])]

-- all possible expressions given a list of Int
exprs :: [Int] -> [Expr]
exprs []  = []
exprs [x] = [Val x]
exprs xs  = [ x | (ls, rs) <- split xs,
                  l        <- exprs ls,
                  r        <- exprs rs,
                  x        <- combine l r]

-- *Problem> exprs [1, 10]
-- [App Mul (Val 1) (Val 10),App Add (Val 1) (Val 10),App Sub (Val 1) (Val 10),App Div (Val 1) (Val 10)]
-- *Problem> exprs [1,2,3]
-- [App Mul (Val 1) (App Mul (Val 2) (Val 3)),App Add (Val 1) (App Mul (Val 2) (Val 3)),App Sub (Val 1) (App Mul (Val 2) (Val 3)),App Div (Val 1) (App Mul (Val 2) (Val 3)),App Mul (Val 1) (App Add (Val 2) (Val 3)),App Add (Val 1) (App Add (Val 2) (Val 3)),App Sub (Val 1) (App Add (Val 2) (Val 3)),App Div (Val 1) (App Add (Val 2) (Val 3)),App Mul (Val 1) (App Sub (Val 2) (Val 3)),App Add (Val 1) (App Sub (Val 2) (Val 3)),App Sub (Val 1) (App Sub (Val 2) (Val 3)),App Div (Val 1) (App Sub (Val 2) (Val 3)),App Mul (Val 1) (App Div (Val 2) (Val 3)),App Add (Val 1) (App Div (Val 2) (Val 3)),App Sub (Val 1) (App Div (Val 2) (Val 3)),App Div (Val 1) (App Div (Val 2) (Val 3)),App Mul (App Mul (Val 1) (Val 2)) (Val 3),App Add (App Mul (Val 1) (Val 2)) (Val 3),App Sub (App Mul (Val 1) (Val 2)) (Val 3),App Div (App Mul (Val 1) (Val 2)) (Val 3),App Mul (App Add (Val 1) (Val 2)) (Val 3),App Add (App Add (Val 1) (Val 2)) (Val 3),App Sub (App Add (Val 1) (Val 2)) (Val 3),App Div (App Add (Val 1) (Val 2)) (Val 3),App Mul (App Sub (Val 1) (Val 2)) (Val 3),App Add (App Sub (Val 1) (Val 2)) (Val 3),App Sub (App Sub (Val 1) (Val 2)) (Val 3),App Div (App Sub (Val 1) (Val 2)) (Val 3),App Mul (App Div (Val 1) (Val 2)) (Val 3),App Add (App Div (Val 1) (Val 2)) (Val 3),App Sub (App Div (Val 1) (Val 2)) (Val 3),App Div (App Div (Val 1) (Val 2)) (Val 3)]

ops :: [Op]
ops = [Mul, Add, Sub, Div]

combine :: Expr -> Expr -> [Expr]
combine l r = [ App o l r | o <- ops ]

-- *Problem> combine (Val 10) (Val 20)
-- [App Mul (Val 10) (Val 20),App Add (Val 10) (Val 20),App Sub (Val 10) (Val 20),App Div (Val 10) (Val 20)]
