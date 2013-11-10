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

value :: Tree a -> Maybe a
value Empty          = Nothing
value (Node _ _ v _) = Just v

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
-- This way, we can factor some functions for those data structures

rebalance :: Tree a -> Tree a
rebalance (Node B (Node R (Node R a x b) y c) z d) = Node R (Node B a x b) y (Node B c z d)
rebalance (Node B a x (Node R b y (Node R c z d))) = Node R (Node B a x b) y (Node B c z d)
rebalance (Node B (Node R a x (Node R b y c)) z d) = Node R (Node B a x b) y (Node B c z d)
rebalance (Node B a x (Node R (Node R b y c) z d)) = Node R (Node B a x b) y (Node B c z d)
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
contains Empty _        = False
contains (Node _ l x r) y = case compare y x of
  EQ -> True
  LT -> contains l y
  GT -> contains r y

toSortedList :: Tree a -> [a]
toSortedList Empty          = []
toSortedList (Node _ l v r) = toSortedList l ++ v : toSortedList r

toList :: Tree a -> [a]
toList Empty          = []
toList (Node _ l x r) = x : toList l ++ toList r

-- Returns how many Reds and Blacks in the given Tree as (redcount, blackcount)
countRB :: Tree a -> (Int, Int)
countRB Empty = (0, 0)
countRB (Node c l _ r) =
  if c == B then (rc, 1 + bc) else (1 + rc, bc)
  where (lrc, lbc) = countRB l
        (rrc, rbc) = countRB r
        rc = lrc + rrc
        bc = lbc + rbc

blackHeight :: Tree a -> Int
blackHeight Empty          = 1
blackHeight (Node c l _ _) = (if c == B then 1 else 0) + blackHeight l

-- FIXME create my own type to permit the behaviour sharing between RBT and BST as a RBT is a BST
-- This function is repeated and adapted from the BST module, this is not DRY!

isBST :: (Ord a) => Tree a -> Bool
isBST Empty          = True
isBST (Node _ l x r) =
  case [value l, value r] of
    [Nothing, Nothing] -> True
    [Nothing, Just z]  -> and [x < z, isBST l, isBST r]
    [Just y, Nothing]  -> and [y <= x, isBST l, isBST r]
    [Just y, Just z]   -> and [y <= x, x < z, isBST l, isBST r]

isRBT :: (Ord a, Eq a) => Tree a -> Bool
isRBT Empty            = True
isRBT t@(Node _ l _ r) = and [isBST t, noRedRed t, blackHeight l == blackHeight r]

-- Returns whether the given tree contains Red-Red nodes or not
noRedRed :: Tree a -> Bool
noRedRed Empty                       = True
noRedRed (Node R (Node R _ _ _) _ _) = False
noRedRed (Node R _ _ (Node R _ _ _)) = False
noRedRed (Node _ l _ r)              = noRedRed l && noRedRed r

-- Returns all paths from root to leaves --
paths :: Tree a -> [[(Color, a)]]
paths Empty                  = [[]]
paths (Node c Empty v Empty) = [[(c, v)]]
paths (Node c l v r) = [ cv : p | p <- paths l ++ paths r ]
                       where cv = (c, v)
