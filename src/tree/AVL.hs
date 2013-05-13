module AVL where

import BinarySearchTree as BST (insert)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

leaf :: a -> Tree a
leaf x = Node x Empty Empty

-- Some examples of structure in code
t1 :: Tree Int
t1 = Node 10 (leaf 8) (leaf 15)

t2 :: Tree Int
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

-- Returns an unsorted list of all values in the given Tree
-- (we need to be able to rebuild the tree from the list)
toList :: Tree a -> [a]
toList Empty        = []
toList (Node x l r) = [x] ++ (toList l) ++ (toList r)

-- *BinarySearchTree> toList t1
-- [4,3,7,5,10]
-- *BinarySearchTree> toList t2
-- [20,15,8,7,11,18,118,35,33,49,60,166]

{--
 Helper fonction that creates a AVL tree from a given list of ordered values
--}
fromList :: Ord a => [a] -> Tree a
fromList []     = Empty
fromList (x:xs) = Node x (fromList lefts) (fromList rights)
                  where p      = (<= x)
                        lefts  = takeWhile p xs
                        rights = dropWhile p xs

-- *BinarySearchTree> (fromList . toList) t1 == t1
-- True
-- *BinarySearchTree> (fromList . toList) t1 == (leaf 1)
-- False
-- *BinarySearchTree> (fromList . toList) t2 == t2
-- True
-- *BinarySearchTree> (fromList . toList) t2 == (leaf 1)
-- False

-- Returns a sorted list of all elements of the given Tree.
-- Note that we can't go back to the origin Tree
toSortedList :: Tree a -> [a]
toSortedList Empty        = []
toSortedList (Node x l r) = toSortedList l ++ [x] ++ toSortedList r

empty :: Tree a -> Bool
empty Empty        = True
empty (Node _ _ _) = False

-- *AVL> leaf 10
-- Node 10 Empty Empty
-- *AVL> empty Empty
-- True
-- *AVL> empty $ leaf 10
-- False

-- Returns whether the given Tree contains the given element or not
contains :: Ord a => Tree a -> a -> Bool
contains Empty _        = False
contains (Node x l r) y = case compare y x of
  EQ -> True
  LT -> contains l y
  GT -> contains r y

-- *AVL> t1
-- Node 10 (Node 8 Empty Empty) (Node 15 Empty Empty)
-- *AVL> contains t1 1
-- False
-- *AVL> contains t1 10
-- True
-- *AVL> contains t1 8
-- True
-- *AVL> contains t1 15
-- True

{--
 Given a tree, compute its height factor (-1, 0 or 1, the tree is well balanced)
--}
heightFactor :: Tree a -> Int
heightFactor Empty = 0
heightFactor (Node _ l r) = (height l) - (height r)

-- *AVL> heightFactor t1
-- 0
-- *AVL> heightFactor t2
-- -1

{--
 returns whether the given tree is h-balanced or not
--}
hBalanced :: Tree a -> Bool
hBalanced Empty        = True
hBalanced (Node x l r) = abs (heightFactor (Node x l r)) <= 1 && hBalanced l && hBalanced r

-- *AVL> hBalanced Empty
-- True
-- *AVL> t1
-- Node 10 (Node 8 Empty Empty) (Node 15 Empty Empty)
-- *AVL> hBalanced t1
-- True
-- *AVL> t2
-- Node 17 (Node 12 (Node 5 (Node 4 Empty Empty) (Node 8 Empty Empty)) (Node 15 Empty Empty)) (Node 115 (Node 32 (Node 30 Empty Empty) (Node 46 (Node 43 Empty Empty) (Node 57 Empty Empty))) (Node 163 (Node 161 Empty Empty) Empty))
-- *AVL> hBalanced t2
-- False

{--
 returns whether the given tree is a binary search tree or not
--}
isBSearchTree :: (Ord a) => Tree a -> Bool
isBSearchTree Empty        = True
isBSearchTree (Node x l r) =
  case map value [l, r] of
    [Nothing, Nothing] -> True
    [Nothing, Just z]  -> and [x < z, isBSearchTree l, isBSearchTree r]
    [Just y, Nothing]  -> and [y <= x, isBSearchTree l, isBSearchTree r]
    [Just y, Just z]   -> and [y <= x, x < z, isBSearchTree l, isBSearchTree r]
  where
    value :: Tree a -> Maybe a
    value Empty        = Nothing
    value (Node v _ _) = Just v

-- *BinarySearchTree> isBSearchTree (Node 10 t2 t1)
-- False
-- *BinarySearchTree> isBSearchTree t1
-- True
-- *BinarySearchTree> isBSearchTree t2
-- True
-- *BinarySearchTree> isBSearchTree (insert t2 1)
-- True
-- *BinarySearchTree> isBSearchTree (insert (insert t2 1) 100)
-- True

{--
 Tells whether the given tree is an AVL or not.
--}
isAVL :: Ord a => Tree a -> Bool
isAVL t = isBSearchTree t && hBalanced t

-- *AVL> isAVL t1
-- True
-- *AVL> isAVL t2
-- True
-- *AVL> isAVL $ Node 10 t1 Empty
-- False

rotateLeft :: Tree a -> Tree a
rotateLeft Empty        = Empty
rotateLeft (Node v lv rt) = case lv of
  Empty              -> (Node v lv rt)
  (Node x lflf lfrt) -> (Node x lflf (Node v lfrt rt))

-- *AVL> t1
-- Node 10 (Node 8 Empty Empty) (Node 15 Empty Empty)
-- *AVL> rotateLeft t1
-- Node 8 Empty (Node 10 Empty (Node 15 Empty Empty))

rotateRight :: Tree a -> Tree a
rotateRight Empty          = Empty
rotateRight (Node v lf rv) = case rv of
  Empty              -> (Node v lf rv)
  (Node x rtlf rtrt) -> (Node x (Node v lf rtlf) rtrt)

-- *AVL> rotateRight t1
-- Node 15 (Node 10 (Node 8 Empty Empty) Empty) Empty
-- *AVL> (rotateLeft . rotateRight) t1 == t1
-- True
-- *AVL> (rotateRight . rotateLeft) t1 == t1
-- True

rebalance :: Tree a -> Tree a
rebalance Empty = Empty
-- rebalance (Node x l r) = let hf = heightFactor (Node x l r) in
--   if hf < -1
--   then Node x (rotateLeft l) r
--   else Node x l (rotateRight r)

{--
  Insert an new ordered value into the tree.
  Note that it preserves the Binary Search tree and the H-balanced properties of an AVL.
--}
insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty x = leaf x
insert (Node x l r) y = undefined


-- insertBS :: (Ord a) => Tree a -> a -> Tree a
-- insertBS Empty x = leaf x
-- insertBS (Node x l r) y = case compare y x of
--   GT -> Node x l (insertBS r y)
--   _  -> Node x (insertBS l y) r

{--
  Remove a node from the tree.
  Note that it preserves the Binary Search tree and the H-balanced properties of an AVL.
--}
remove :: (Ord a) => Tree a -> a -> Tree a
remove  = undefined

{--
 Deletes the maximum element in a given Tree.
 Note that this implementation works only on non Empty Trees
--}
deleteMax :: Tree a -> (a, Tree a)
deleteMax = undefined

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
