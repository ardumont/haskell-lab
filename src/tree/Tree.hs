module Tree where

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
