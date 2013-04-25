module Ch10 where

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7))

occurs :: Eq a => Tree a -> a -> Bool
occurs (Leaf n)     m = n == m
occurs (Node l n r) m = (n == m) ||
                        (occurs l m) ||
                        (occurs r m)

-- *Ch10> map (occurs t) [0..8]
-- [False,True,True,True,True,True,True,True,False]

flatten :: Tree a -> [a]
flatten (Leaf n)     = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

-- for search trees (left nodes have inferior values, right nodes superior values)
-- so we can rewrite occurs to be faster

occursST :: (Eq a, Ord a) => Tree a -> a -> Bool
occursST (Leaf n)     m = m == n
occursST (Node l n r) m | m == n    = True
                        | m < n     = occursST l m
                        | otherwise = occursST r m

type Assoc k v = [(k, v)]

find :: Eq a => a -> Assoc a b -> b
find k t = head [ v | (k', v) <- t, k == k' ]

-- Tautology: logical propositions that are always true

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          deriving Show

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply
      (Var 'A') (Var 'B'))) (Var 'B')

p6 :: Prop
p6 = And (Var 'a') (Var 'b')

p7 :: Prop
p7 = Not p6

p8 :: Prop
p8 = Imply p6 p7
