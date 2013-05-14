module AVL where

-- import Test.QuickCheck
-- import Test.QuickCheck.All

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
-- *AVL> rotateLeft t5
-- Node 3 (Node 2 (Node 1 Empty Empty) Empty) (Node 6 (Node 4 Empty Empty) (Node 7 Empty Empty))
-- *AVL> rotateLeft t5 == t6
-- True

rotateRight :: Tree a -> Tree a
rotateRight Empty                        = Empty
rotateRight (Node v lf Empty)            = (Node v lf Empty)
rotateRight (Node v lf (Node x rtl rtr)) = (Node x (Node v lf rtl) rtr)

t5 :: Tree Int
t5 = Node 6 (Node 3
               (Node 2
                  (leaf 1)
                  Empty)
               (leaf 4))
            (leaf 7)

t6 :: Tree Int
t6 = Node 3 (Node 2
               (leaf 1)
               Empty)
            (Node 6
               (leaf 4)
               (leaf 7))

-- *AVL> rotateRight t1
-- Node 15 (Node 10 (Node 8 Empty Empty) Empty) Empty
-- *AVL> (rotateLeft . rotateRight) t1 == t1
-- True
-- *AVL> (rotateRight . rotateLeft) t1 == t1
-- True
-- *AVL> rotateRight t6 == t5
-- True

-- Given an unbalanced avl, compute the rebalanced avl at the given level
rebalance :: Tree a -> Tree a
rebalance n =
  let hf = heightFactor n in
  if abs hf <= 1
  then n
  else if hf < -2
  then (rotateLeft . rotateRight) n
       else if hf < -1
            then rotateRight n
            else if hf > 2
                 then (rotateRight . rotateLeft) n
                 else (rotateLeft n)

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
ins :: (Ord a) => Tree a -> a -> Tree a
--insert t v = rebalance $ BST.insert t v
ins Empty v = leaf v
ins (Node x l r) y
  | x < y      = rebalance $ Node x l (ins r y)
  | otherwise  = rebalance $ Node x (ins l y) r

--prop_avl = (\ t -> abs (heightFactor t) <= 1)

-- adding
--main = do
-- verboseCheckWith stdArgs { maxSuccess = 1000, maxSize = 5 } prop_avl

-- *AVL> isAVL $ ins t1 3
-- True
-- *AVL> isAVL $ ins t1 2
-- True
-- *AVL> isAVL $ ins t1 10
-- True
-- *AVL> isAVL $ ins t1 1100
-- True
-- *AVL> isAVL $ ins (ins t1 1100) 1200
-- True
-- *AVL> isAVL $ ins (ins (ins t1 1100) 1200) 1300
-- True
-- *AVL> ins (ins (ins t1 1100) 1200) 1300
-- Node 7 (Node 4 (Node 3 Empty Empty) (Node 5 Empty Empty)) (Node 1100 (Node 10 Empty Empty) (Node 1200 Empty (Node 1300 Empty Empty)))
-- *AVL> ins (ins (ins (ins t1 1100) 1200) 1300) 1400
-- Node 7 (Node 4 (Node 3 Empty Empty) (Node 5 Empty Empty)) (Node 1100 (Node 10 Empty Empty) (Node 1300 (Node 1200 Empty Empty) (Node 1400 Empty Empty)))
-- *AVL> isAVL (ins (ins (ins (ins t1 1100) 1200) 1300) 1400)
-- True

{--
  Remove a node from the tree.
  Note that it preserves the Binary Search tree and the H-balanced properties of an AVL.
--}
remove :: (Ord a) => Tree a -> a -> Tree a
remove Empty _ = Empty
remove (Node x l r) y
  | x < y     = AVL.remove r y
  | x > y     = AVL.remove l y
  | otherwise = case deleteMax l of
    (Just z, t)  -> rebalance $ Node z t r
    (Nothing, _) -> Empty

-- *AVL> isAVL (BST.remove (ins (ins (ins (ins t1 1100) 1200) 1300) 1400) 1100)
-- False
-- *AVL> isAVL (AVL.remove (ins (ins (ins (ins t1 1100) 1200) 1300) 1400) 1100)
-- True

{--
 Breadth first traversal
--}
breadth :: Tree a -> [a]
breadth t =
  bf [t] []
  where
    bf :: [Tree a] -> [a] -> [a]
    bf [] q                  = q
    bf (Empty : ns)        q = bf ns q                   -- drop the empty nodes
    bf ((Node x l r) : ns) q = bf (l : r : ns) (q ++ [x])

-- *AVL> t1
-- Node 4 (Node 3 Empty Empty) (Node 7 (Node 5 Empty Empty) (Node 10 Empty Empty))
-- *AVL> breadth t1
-- [4,3,7,5,10]
-- *AVL> breadth (ins (ins (ins (ins t1 1100) 1200) 1300) 1400)
-- [7,4,3,5,1100,10,1300,1200,1400]

{--
  breadth first traversal based filtering.
  returns the list of all elements satisfying the given predicate
--}
filterT :: (a -> Bool) -> Tree a -> [a]
filterT p = (filter p) . breadth

-- *AVL> filterT (<= 3) t1
-- [3]
-- *AVL> filterT (<= 1000) t1
-- [4,3,7,5,10]
-- *AVL> filterT (<= 1000) $ (ins (ins (ins (ins t1 1100) 1200) 1300) 1400)
-- [7,4,3,5,10]
-- *AVL> filterT (<= 1100) $ (ins (ins (ins (ins t1 1100) 1200) 1300) 1400)
-- [7,4,3,5,1100,10]

{--
 Breadth first traversal based implementation of exist
--}
exist:: Eq a => a -> Tree a -> Bool
exist x = not . null . filterT (== x)

-- *AVL> t1
-- Node 4 (Node 3 Empty Empty) (Node 7 (Node 5 Empty Empty) (Node 10 Empty Empty))
-- *AVL> exist 1 t1
-- False
-- *AVL> exist 3 t1
-- True
