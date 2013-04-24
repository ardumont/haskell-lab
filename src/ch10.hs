module Ch10 where

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7))

occurs :: Eq a => Tree a -> a -> Bool
occurs (Leaf n)     m = n == m
occurs (Node l n r) m = or [(n == m), (occurs l m), (occurs r m)]

-- *Ch10> map (occurs t) [0..8]
-- [False,True,True,True,True,True,True,True,False]
