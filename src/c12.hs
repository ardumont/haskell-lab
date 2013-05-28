module C12 where

fibo :: [Integer]
fibo = 0:1:[ x+y | (x,y) <- zip fibo (tail fibo)]

-- *C12> take 20 fibo
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]

fibs :: Int -> Integer
fibs n = fibo !! n

-- *C12> fibs 10
-- 34
-- *C12> fibs 100
-- 218922995834555169026
-- *C12> fibs 101
-- 354224848179261915075

-- *C12> head . dropWhile (<= 1000) $ fibo
-- 1597

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

p :: Show a => Tree a -> IO ()
p = (mapM_ putStrLn) . treeIndent
  where
    treeIndent Leaf          = ["-- /-"]
    treeIndent (Node lb v rb) =
      ["--" ++ (show v)] ++
      map ("  |" ++) ls ++
      ("  `" ++ r) : map ("   " ++) rs
      where
        (r:rs) = treeIndent $ rb
        ls     = treeIndent $ lb

repeatTree :: a -> Tree a
repeatTree v =
  Node t v t
  where t = repeatTree v

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _     = Leaf
takeTree _ Leaf  = Leaf
takeTree n (Node l x r) = Node (takeTree (n-1) l) x (takeTree (n-1) r)

-- *C12> p $ takeTree 3 (repeatTree 0)
-- --0
--   |--0
--   |  |--0
--   |  |  |-- /-
--   |  |  `-- /-
--   |  `--0
--   |     |-- /-
--   |     `-- /-
--   `--0
--      |--0
--      |  |-- /-
--      |  `-- /-
--      `--0
--         |-- /-
--         `-- /-

replicateTree :: Int -> a -> Tree a
replicateTree n = (takeTree n) . repeatTree

-- *C12> p $ replicateTree 3 0
-- --0
--   |--0
--   |  |--0
--   |  |  |-- /-
--   |  |  `-- /-
--   |  `--0
--   |     |-- /-
--   |     `-- /-
--   `--0
--      |--0
--      |  |-- /-
--      |  `-- /-
--      `--0
--         |-- /-
--         `-- /-

data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero y = y
add (Succ x) y = Succ (add x y)

-- base case, immediate
-- add Zero y = y

-- proof add n Zero
-- add Zero Zero = Zero
-- add n Zero = Succ (add n Zero)
--            = Succ n
--ok

-- proof associativity of: add x (add y z) = add (add x y) z
-- base case
-- add Zero (add y z) = add y z
--                    = add (add Zero y) z)
-- ok

-- inductive case
-- add (Succ x) (add y z) = Succ (add x (add y z))
--                        = Succ (add (add x y) z) -- by induction
--                        = add (Succ (add x y) z)
--                        = add (add (Succ x) y) z
-- ok

rep :: Int -> a -> [a]
rep 0 _ = []
rep n x = x : rep (n-1) x

-- *C12> rep 3 0
-- [0,0,0]

-- proof: length (rep n _) == n
-- base case:
-- length (rep 0 x) = length [] = 0
-- ok

-- inductive case:
-- length (rep n+1 x) = length (x : rep n x) -- distributivity
--                  = 1 + length (rep n x)   -- induction
--                  = 1 + n
-- ok

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- *C12> rev [1..3]
-- [3,2,1]

-- proof: rev (rev xs) = xs
-- base case:
-- rev (rev []) = rev [] = []
-- ok
-- inductive case:
-- rev (rev (x:y:xs)) = rev ((y:xs) ++ [x])
--                    = rev xs ++ [y] ++ [x]
--                    = rev xs ++ ([y] ++ [x])
--                    = rev xs ++ [y, x]
--                    = rev (y:x:xs)

-- proof: rev' xs ys = rev xs ++ [ys]
-- base case: rev' [] ys = rev [] ++ [ys]
--                       = [ys]

-- inductive case: rev' (x:xs) ys = rev (x:xs) ++ [ys]
--                                = rev xs ++ [x] ++ [ys]
--                                = rev xs ++ (x:ys)

rev' :: [a] -> [a]
rev' l = rev'' l []
         where
           rev'' :: [a] -> [a] -> [a]
           rev'' [] ys     = ys
           rev'' (x:xs) ys = rev'' xs (x:ys)

data T = L Int | N T T

flatten :: T -> [Int]
flatten (L x) = [x]
flatten (N l r) = flatten l ++ flatten r

-- proof: flatten' t ns = flatten t ++ ns
-- base case: flatten' (Leaf x) ns = flatten (Leaf x) ++ ns
--                                 = [x] ++ ns
--                                 = x:ns

-- inductive case: flatten' (N l r) ns = flatten (N l r) ++ ns
--                                     = (flatten l ++ flatten r) ++ ns
--                                     = flatten l ++ (flatten r ++ ns)
--                                     = flatten l ++ (flatten' r ns)
--                                     = flatten' l (flatten' r ns)

flatten' :: T -> [Int]
flatten' t = flatten'' t []
             where flatten'' :: T -> [Int] -> [Int]
                   flatten'' (L n) ns   = (n:ns)
                   flatten'' (N l r) ns = flatten'' l (flatten'' r ns)

-- *C12> flatten' (N (N (L 0) (L 1)) (L 2))
-- [0,1,2]

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val x)   = x
eval (Add x y) = eval x + eval y

type Stack = [Int]
type Code = [Op]
data Op = PUSH Int | ADD deriving (Show)

exec :: Code -> Stack -> Stack
exec [] s             = s
exec (PUSH x:c) s     = exec c (x:s)
exec (ADD:c) (x:y:xs) = exec c (x+y:xs)

-- *C12> exec [PUSH 1,PUSH 2,ADD] []
-- [3]
-- *C12> exec [PUSH 10,PUSH 11,PUSH 12,ADD,ADD] []
-- [33]

comp :: Expr -> Code
comp (Val x)   = [PUSH x]
comp (Add l r) = (comp l) ++ (comp r) ++ [ADD]

-- *C12> comp (Add (Val 10) (Add (Val 11) (Val 12)))
-- [PUSH 10,PUSH 11,PUSH 12,ADD,ADD]
