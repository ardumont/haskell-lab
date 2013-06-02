module RBT where

-- import qualified Data.List as L

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
