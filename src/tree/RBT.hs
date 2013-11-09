module RBT where

import Data.List (foldl')

data Color  = R | B deriving (Eq, Show)
data Tree a = Empty | Node Color (Tree a) a (Tree a) deriving (Eq, Show)

-- All empty nodes considered to be black (so no color field for Empty)
-- Every RBT satisfy the following two balance invariants:
--  - No red node has a red child
--  - Every path from the root to an empty node contains the same number of black nodes

left :: Tree a -> Tree a
left Empty = undefined
left (Node _ l _ _) = l

right :: Tree a -> Tree a
right Empty = undefined
right (Node _ _ _ r) = r

color :: Tree a -> Color
color Empty = undefined
color (Node c _ _ _) = c

pp :: Show a => Tree a -> IO ()
pp = (mapM_ putStrLn) . treeIndent
  where
    treeIndent Empty          = ["-- /-"]
    treeIndent (Node c lb v rb) =
      ["--" ++ (show c) ++ " " ++ (show v)] ++
      map ("  |" ++) ls ++
      ("  `" ++ r) : map ("   " ++) rs
      where
        (r:rs) = treeIndent $ rb
        ls     = treeIndent $ lb

makeLeaf :: Color -> a -> Tree a
makeLeaf c v = Node c Empty v Empty

makeLeft :: Color -> a -> Tree a -> Tree a
makeLeft c v l = Node c l v Empty

makeRight :: Color -> a -> Tree a -> Tree a
makeRight c v r = Node c Empty v r

rbt0 :: Tree Int
rbt0 = Node B
       (Node R (makeLeaf B 0) 1 (makeLeft B 3 (makeLeaf R 2)))
       4
       (makeLeaf B 5)

-- *RBT> rbt0
-- Node B (Node R (Node B Empty 0 Empty) 1 (Node B (Node R Empty 2 Empty) 3 Empty)) 4 (Node B Empty 5 Empty)

-- *RBT> pp rbt0
-- --B 4
--   |--R 1
--   |  |--B 0
--   |  |  |-- /-
--   |  |  `-- /-
--   |  `--B 3
--   |     |--R 2
--   |     |  |-- /-
--   |     |  `-- /-
--   |     `-- /-
--   `--B 5
--      |-- /-
--      `-- /-

rbt1 :: Tree Int
rbt1 = Node B
       (Node B (makeLeaf R 1) 3 (makeLeaf R 2))
       4
       (Node B (makeLeaf R 5) 6 (makeLeaf R 7))

-- *RBT> rbt1
-- Node B (Node B (Node R Empty 1 Empty) 3 (Node R Empty 2 Empty)) 4 (Node B (Node R Empty 5 Empty) 6 (Node R Empty 7 Empty))

-- *RBT> pp rbt1
-- --B 4
--   |--B 3
--   |  |--R 1
--   |  |  |-- /-
--   |  |  `-- /-
--   |  `--R 2
--   |     |-- /-
--   |     `-- /-
--   `--B 6
--      |--R 5
--      |  |-- /-
--      |  `-- /-
--      `--R 7
--         |-- /-
--         `-- /-

rbt2 :: Tree Int
rbt2 = Node B
              (makeLeaf B 0)
              1
              (Node R
                     (Node B (makeLeaf R 3) 4 (makeLeaf R 5))
                      6
                     (makeLeaf B 7))

-- *RBT> rbt2
-- Node B (Node B Empty 0 Empty) 1 (Node R (Node B (Node R Empty 3 Empty) 4 (Node R Empty 5 Empty)) 6 (Node B Empty 7 Empty))

-- *RBT> pp rbt2
-- --B 1
--   |--B 0
--   |  |-- /-
--   |  `-- /-
--   `--R 6
--      |--B 4
--      |  |--R 3
--      |  |  |-- /-
--      |  |  `-- /-
--      |  `--R 5
--      |     |-- /-
--      |     `-- /-
--      `--B 7
--         |-- /-
--         `-- /-

-- FIXME: Improve the data type to create a constructor of meta data (color + value for RBT and just value for AVL)
-- This way, we can factor some joint functions for those data structure

rebalance :: Tree a -> Tree a
rebalance (Node B (Node R (Node R zl zv zr) xv xr) yv yr) = Node R (Node B zl zv zr) xv (Node B xr yv yr)
rebalance (Node B xl xv (Node R yl yv (Node R zl zv zr))) = Node R (Node B xl xv yl) yv (Node B zr zv zl)
rebalance (Node B (Node R yl yv (Node R zl zv zr)) xv xr) = Node R (Node B yl yv zl) zv (Node B zr xv xr)
rebalance (Node B xl xv (Node R (Node R zl zv zr) yv yr)) = Node R (Node B xl xv zl) zv (Node B zr yv yr)
rebalance r = r

insert :: Ord a => Tree a -> a -> Tree a
insert t y = Node B l v r -- force the root to be black
             where Node _ l v r = ins t
                   ins Empty = makeLeaf R y
                   ins n@(Node c tl x tr)
                     | x == y     = n
                     | x < y      = rebalance $ Node c tl x (ins tr)
                     | otherwise  = rebalance $ Node c (ins tl) x tr

-- Creates a new Red-Black Tree from a given list
fromList :: Ord a => [a] -> Tree a
fromList = foldl' insert Empty

contains :: Ord a => Tree a -> a -> Bool
contains = undefined

toSortedList :: Tree a -> [a]
toSortedList Empty = []
toSortedList (Node _ l v r) = toSortedList l ++ v : toSortedList r

toList :: Tree a -> [a]
toList Empty          = []
toList (Node _ l x r) = x : (toList l) ++ (toList r)

{-- Returns how many Reds and Blacks in the given Tree as (redcount, blackcount) --}
countRB :: (Num b, Num c) => Tree a -> (b, c)
countRB Empty = (0, 0)
countRB (Node B l _ r) =
  (rc, 1 + bc)
  where (lrc, lbc) = countRB l
        (rrc, rbc) = countRB r
        rc = lrc + rrc
        bc = lbc + rbc
countRB (Node R l _ r) =
  (1 + rc, bc)
  where (lrc, lbc) = countRB l
        (rrc, rbc) = countRB r
        rc = lrc + rrc
        bc = lbc + rbc

-- *RBT> rbt0
-- Node B (Node R (Node B Empty 0 Empty) 1 (Node B (Node R Empty 2 Empty) 3 Empty)) 4 (Node B Empty 5 Empty)
-- *RBT> rbt1
-- *RBT> countRB rbt0
-- (2,4)
-- Node B (Node B (Node R Empty 1 Empty) 3 (Node R Empty 2 Empty)) 4 (Node B (Node R Empty 5 Empty) 6 (Node R Empty 7 Empty))
-- *RBT> countRB rbt1
-- (4,3)
-- *RBT> rbt2
-- Node B (Node B Empty 0 Empty) 1 (Node R (Node B (Node R Empty 3 Empty) 4 (Node R Empty 5 Empty)) 6 (Node B Empty 7 Empty))
-- *RBT> countRB rbt2
-- (3,4)

isRBTree :: Eq a => Tree a -> Bool
isRBTree = undefined
-- isRBTree (Node R _ (Node R _ _ _) _) = False
-- isRBTree (Node R _ _ (Node R _ _ _)) = False
-- isRBTree (Node _ _ l r) =
--   and [(lbc == rbc), (isRBTree l), (isRBTree r)]
--   where (_, lbc) = countRB l
--         (_, rbc) = countRB r

{-- Returns whether the given tree contains Red-Red nodes or not --}
noRedRed :: Tree a -> Bool
noRedRed Empty = undefined

-- *RBT> color rbt1
-- B
-- *RBT> color rbt0
-- B
-- *RBT> color rbt2
-- B

{-- Returns all paths from root to leaves --}
paths :: Tree a -> [[(Color, a)]]
paths = undefined
