module RBT where

-- import qualified Data.List as L

-- Definitions:

-- non branching node: node with exactly 0 or 1 children (only one direction possible)

-- RB-adjacent: 2 nodes are considered adjacents if one is the child of the other

-- path: list of 1 or more nodes in a BT where every node in the list is adjacent to the one after it

-- simple path: path without duplicates

-- descendant: a node p is a descendant of a node q if
-- - both p and q are the same node
-- - or if p is located in one of the subtrees of q

-- RB tree: binary search tree in which every node has been labeled with a color R or B.
-- with those colors distributed according to the following RB balancing rules:
-- - no R node has a R child
-- - every simple path from a given node to one of its non-branching node descendants contains the same number of B nodes
-- - every root node is black (optional but simplify the operations)

data Color  = R | B deriving (Eq, Show)
data Tree a = Empty | Node Color a (Tree a) (Tree a) deriving (Eq, Show)

left :: Tree a -> Tree a
left Empty = undefined
left (Node _ _ l _) = l

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
    treeIndent (Node c v lb rb) =
      ["--" ++ (show c) ++ " " ++ (show v)] ++
      map ("  |" ++) ls ++
      ("  `" ++ r) : map ("   " ++) rs
      where
        (r:rs) = treeIndent $ rb
        ls     = treeIndent $ lb

makeLeaf :: Color -> a -> Tree a
makeLeaf c v = Node c v Empty Empty

makeLeft :: Color -> a -> Tree a -> Tree a
makeLeft c v l = Node c v l Empty

makeRight :: Color -> a -> Tree a -> Tree a
makeRight c v r = Node c v Empty r

rbt0 :: Tree Int
rbt0 = Node B 4
       (Node R 1 (makeLeaf B 0) (makeLeft B 3 (makeLeaf R 2)))
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
rbt1 = Node B 4
       (Node B 3 (makeLeaf R 1) (makeLeaf R 2))
       (Node B 6 (makeLeaf R 5) (makeLeaf R 7))

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
rbt2 = Node B 1
              (makeLeaf B 0)
              (Node R 6
                     (Node B 4 (makeLeaf R 3) (makeLeaf R 5))
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

rotateR :: Tree a -> Tree a
rotateR Empty                     = Empty
rotateR n@(Node _ _ Empty _)        = n
rotateR (Node yc yv (Node xc xv xl xr) yr) = (Node xc xv xl (Node yc yv xr yr))

rotateL :: Tree a -> Tree a
rotateL Empty                     = Empty
rotateL n@(Node _ _ _ Empty)        = n
rotateL (Node xc xv xl (Node yc yv yl yr)) = (Node yc yv (Node xc xv xl yl) yr)

insert :: Ord a => Tree a -> a -> Tree a
insert = undefined

contains :: Ord a => Tree a -> a -> Bool
contains = undefined

toSortedList :: Tree a -> [a]
toSortedList = undefined

toList :: Tree a -> [a]
toList = undefined

{-- Returns how many Reds and Blacks in the given Tree as (redcount, blackcount) --}
countRB :: (Num b, Num c) => Tree a -> (b, c)
countRB Empty = (0, 0)
countRB (Node B _ l r) =
  (rc, 1 + bc)
  where (lrc, lbc) = countRB l
        (rrc, rbc) = countRB r
        rc = lrc + rrc
        bc = lbc + rbc
countRB (Node R _ l r) =
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

{-- Creates a new Red-Black Tree from a given list --}
fromList :: Ord a => [a] -> Tree a
fromList = undefined

isRBTree :: Eq a => Tree a -> Bool
isRBTree Empty = True
isRBTree (Node R _ (Node R _ _ _) _) = False
isRBTree (Node R _ _ (Node R _ _ _)) = False
isRBTree (Node _ _ l r) =
  and [(lbc == rbc), (isRBTree l), (isRBTree r)]
  where (_, lbc) = countRB l
        (_, rbc) = countRB r

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
