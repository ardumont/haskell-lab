module AVL where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq,Show)

leaf :: a -> Tree a
leaf x = Node x Empty Empty

-- Some examples of structure in code
t1 = Node 10 (leaf 8) (leaf 15)

t2 = Node 17 (Node 12 (Node 5 (leaf 4) (leaf 8)) (leaf 15))
             (Node 115
                     (Node 32 (leaf 30) (Node 46 (leaf 43) (leaf 57)))
                              (Node 163 (leaf 161) Empty))

-- The size of the tree is taken to be the number n of internal nodes
--(those with two children)
size :: Num a => Tree b -> a
size Empty        = 0
size (Node _ l r) = 1 + size l + size r

-- *AVL> size t1
-- 3
-- *AVL> size t2
-- 14

-- maximum distance from any node to the root
height :: (Ord a, Num a) => Tree t -> a
height Empty            = -1
height (Node _ _ Empty) = 1
height (Node _ Empty _) = 1
height (Node _ l r)     = 1 + max (height l) (height r)

-- *AVL> height t1
-- 2
-- *AVL> height t2
-- 5

-- Return a unsorted list, but we can go back to the origin Tree from this list
toList :: Tree a -> [a]
toList = undefined

-- Returns a sorted list of all elements of the given Tree. Note that we can't go back to the origin Tree
toSortedList :: Tree a -> [a]
toSortedList  = undefined

empty :: Tree a -> Bool
empty  = undefined

contains :: Ord a =>  Tree a -> a -> Bool
contains  = undefined

{--
  Insert an new ordred value into the tree.
  Note that it preserves the Binary Search tree propertie,
  and the H-balanced propertie of an AVL.
--}
insert :: (Ord a) => Tree a -> a -> Tree a
insert = undefined

remove :: (Ord a) => Tree a -> a -> Tree a
remove  = undefined

{--
 Deletes the maximum element in a given Tree.
 Note that this implementation works only on non Empty Trees
--}
deleteMax :: Tree a -> (a, Tree a)
deleteMax = undefined

{--
 returns whether the given tree is a binary search tree or not
--}
isBSearchTree :: (Ord a) => Tree a -> Bool
isBSearchTree  = undefined

{--
 Tells whether the given tree is an AVL or not.
--}
isAVL :: Tree a -> Bool
isAVL = undefined

{--
 Helper fonction that creates a AVL tree from a given list of ordered values
--}
fromList :: (Ord a) => [a] -> Tree a
fromList = undefined

{--
 Breadth first traversal
--}
breadth :: [Tree a] -> [a]
breadth = undefined

{--
  breadth first traversal based filtring.
  returns the list of all elements satisfying the given predicate
--}
filterT :: (a -> Bool) -> Tree a -> [a]
filterT = undefined

{--
 Breadth first traversal based implementation of exist
--}
exist:: Eq a => a -> Tree a -> Bool
exist = undefined
