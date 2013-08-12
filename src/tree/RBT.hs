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

data Color  = R | B deriving (Eq, Show)
data Tree a = Empty | Node Color (Tree a) a (Tree a) deriving (Eq, Show)

insert :: Ord a => Tree a -> a -> Tree a
insert = undefined

contains :: Ord a => Tree a -> a -> Bool
contains = undefined

toSortedList :: Tree a -> [a]
toSortedList = undefined

toList :: Tree a -> [a]
toList = undefined

{-- Returns how many Reds and Blacks in the given Tree as (redcount, blackcount) --}
countRB ::(Num b, Num c) => Tree a -> (b, c)
countRB = undefined

{-- Creates a new Red-Black Tree from a given list --}
fromList :: Ord a => [a] -> Tree a
fromList = undefined

isRBTree :: Eq a => Tree a -> Bool
isRBTree = undefined

{-- Returns whether the given tree contains Red-Red nodes or not --}
noRedRed :: Tree a -> Bool
noRedRed = undefined

color :: Tree a -> Color
color = undefined

{-- Returns all paths from root to leaves --}
paths :: Tree a -> [[(Color, a)]]
paths = undefined
