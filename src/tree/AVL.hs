module AVL where

import BinarySearchTree as BST

-- Some examples of structure in code
t3 :: Tree Int
t3 = Node 10 (leaf 8) (leaf 15)

t4 :: Tree Int
t4 = Node 17 (Node 12 (Node 5 (leaf 4) (leaf 8))
                      (leaf 15))
             (Node 115
                     (Node 32 (leaf 30)
                              (Node 46 (leaf 43)
                                       (leaf 57)))
                     (Node 163 (leaf 161)
                               Empty))

-- *AVL> size t1
-- 3
-- *AVL> size t2
-- 14

-- maximum distance from any node to the root
height :: (Ord a, Num a) => Tree t -> a
height Empty            = -1
height (Node _ l r)     = 1 + max (height l) (height r)

-- *AVL> height t1
-- 2
-- *AVL> height t2
-- 5

empty :: Tree a -> Bool
empty Empty        = True
empty (Node _ _ _) = False

-- *AVL> leaf 10
-- Node 10 Empty Empty
-- *AVL> empty Empty
-- True
-- *AVL> empty $ leaf 10
-- False

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
 Tells whether the given tree is an AVL or not.
--}
isAVL :: Ord a => Tree a -> Bool
isAVL t = isBSearchTree t && hBalanced t

left :: Tree a -> Tree a
left Empty = Empty
left (Node _ l _) = l

right :: Tree a -> Tree a
right Empty = Empty
right (Node _ _ r) = r

-- *AVL> isAVL t1
-- True
-- *AVL> isAVL t2
-- True
-- *AVL> isAVL $ Node 10 t1 Empty
-- False

rotateLeft :: Tree a -> Tree a
rotateLeft Empty                       = Empty
rotateLeft (Node v Empty r)            = (Node v Empty r)
rotateLeft (Node v (Node x lfl lfr) r) = (Node x lfl (Node v lfr r))

-- *AVL> t1
-- Node 10 (Node 8 Empty Empty) (Node 15 Empty Empty)
-- *AVL> rotateLeft t1
-- Node 8 Empty (Node 10 Empty (Node 15 Empty Empty))

rotateRight :: Tree a -> Tree a
rotateRight Empty                        = Empty
rotateRight (Node v lf Empty)            = (Node v lf Empty)
rotateRight (Node v lf (Node x rtl rtr)) = (Node x (Node v lf rtl) rtr)

-- *AVL> rotateRight t1
-- Node 15 (Node 10 (Node 8 Empty Empty) Empty) Empty
-- *AVL> (rotateLeft . rotateRight) t1 == t1
-- True
-- *AVL> (rotateRight . rotateLeft) t1 == t1
-- True

-- Given an unbalanced avl, compute the rebalanced avl at the given level
rebalance :: Tree a -> Tree a
rebalance n =
  let hf = heightFactor n in
  if abs hf <= 1
  then n
  else if hf < -1
  then rebalance $ (rotateRight n)
  else rebalance $ (rotateLeft n)

-- *AVL> t1
-- Node 10 (Node 8 Empty Empty) (Node 15 Empty Empty)
-- *AVL> rebalance $ rotateLeft t1
-- Node 10 (Node 8 Empty Empty) (Node 15 Empty Empty)
-- *AVL> rebalance $ rotateRight t1
-- Node 10 (Node 8 Empty Empty) (Node 15 Empty Empty)
-- *AVL> rotateLeft t2
-- Node 12 (Node 5 (Node 4 Empty Empty) (Node 8 Empty Empty)) (Node 17 (Node 15 Empty Empty) (Node 115 (Node 32 (Node 30 Empty Empty) (Node 46 (Node 43 Empty Empty) (Node 57 Empty Empty))) (Node 163 (Node 161 Empty Empty) Empty)))
-- *AVL> heightFactor $ rotateLeft t2
-- -3
-- *AVL> heightFactor $ rebalance $ rotateLeft t2
-- -1

{--
  Insert an new ordered value into the tree.
  Note that it preserves the Binary Search tree and the H-balanced properties of an AVL.
--}
insert :: (Ord a) => Tree a -> a -> Tree a
insert = undefined
--insert t v = rebalance $ BST.insert t v

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
