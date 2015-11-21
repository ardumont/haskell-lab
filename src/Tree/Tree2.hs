module Tree.Tree2 where

data Tree = Leaf Int | Node Tree Tree deriving Show

t :: Tree
t = Node (Node (Leaf 1) (Leaf 3)) (Node (Leaf 5) (Leaf 7))

nbLeaves :: Tree -> Int
nbLeaves (Leaf _) = 1
nbLeaves (Node l r) = nbLeaves l + nbLeaves r

-- *Tree2> nbLeaves t
-- 4

balanced :: Tree -> Bool
balanced (Leaf _) = True -- Leaf is trivially balanced
balanced (Node l r) = let nl = nbLeaves l
                          nr = nbLeaves r in
                      abs (nl - nr) <= 1 &&
                      balanced l &&
                      balanced r

-- *Tree2> balanced t
-- True
-- *Tree2> balanced (Node t (Leaf 1))
-- False
-- *Tree2> balanced (Leaf 1)
-- True
-- *Tree2> balanced $ Node (Leaf 1) (Leaf 2)
-- True
-- *Tree2> balanced $ Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
-- True
-- *Tree2> balanced $ Node (Leaf 1) (Node (Leaf 2) (Node (Leaf 4) (Leaf 3)))
-- False

split :: [a] -> ([a], [a])
split l = splitAt n l where n = (length l) `div` 2

-- *Tree2> split [1..11]
-- ([1,2,3,4,5],[6,7,8,9,10,11])
-- *Tree2> split [1]
-- ([],[1])
-- *Tree2> split [1,2]
-- ([1],[2])
-- *Tree2> split [1,2,3]
-- ([1],[2,3])

balance :: [Int] -> Tree
balance [x]     = Leaf x
balance ls = let (l, r) = split ls in
  Node (balance l) (balance r)

-- *Tree2> map balanced (map balance [ [1..i] | i <- [1..10] ])
-- [True,True,True,True,True,True,True,True,True,True]
-- *Tree2> map balanced (map balance [ [1..i] | i <- [1..100] ])
-- [True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
-- *Tree2> filter (== False) (map balanced (map balance [ [1..i] | i <- [1..100] ]))
-- []
