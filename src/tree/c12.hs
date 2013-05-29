module C12 where

data Tree = Leaf Int | Node Tree Tree

leaves :: Tree -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

-- *C12> leaves (Node (Node (Leaf 0) (Node (Leaf 1) (Leaf 2))) (Leaf 3))
-- 4

nodes :: Tree -> Int
nodes (Leaf _) = 0
nodes (Node l r) = 1 + nodes l + nodes r

-- *C12> nodes (Node (Node (Leaf 0) (Node (Leaf 1) (Leaf 2))) (Leaf 3))
-- 3
