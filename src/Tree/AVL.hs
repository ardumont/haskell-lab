module Tree.AVL where

-- import Test.QuickCheck
-- import Test.QuickCheck.All

import           Tree.BinarySearchTree as BST

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

hFactor :: Tree a -> Tree a -> Int
hFactor l r = (height l) - (height r)

{--
 Given a tree, compute its height factor (-1, 0 or 1, the tree is well balanced)
--}
heightFactor :: Tree a -> Int
heightFactor Empty = 0
heightFactor (Node _ l r) = hFactor l r

-- *AVL> heightFactor t1
-- 0
-- *AVL> heightFactor t2
-- -1

{--
 returns whether the given tree is h-balanced or not
--}
hBalanced :: Tree a -> Bool
hBalanced Empty        = True
hBalanced n@(Node _ l r) = abs (heightFactor n) <= 1 && hBalanced l && hBalanced r

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

rotateR :: Tree a -> Tree a
rotateR Empty                     = Empty
rotateR n@(Node _ Empty _)        = n
rotateR (Node v (Node x ll lr) r) = (Node x ll (Node v lr r))

-- *AVL> t1
-- Node 4 (Node 3 Empty Empty) (Node 7 (Node 5 Empty Empty) (Node 10 Empty Empty))
-- *AVL> rotateR t1
-- Node 3 Empty (Node 4 Empty (Node 7 (Node 5 Empty Empty) (Node 10 Empty Empty)))

rotateL :: Tree a -> Tree a
rotateL Empty                     = Empty
rotateL n@(Node _ _ Empty)        = n
rotateL (Node v l (Node x rl rr)) = (Node x (Node v l rl) rr)

-- *AVL> t1
-- Node 4 (Node 3 Empty Empty) (Node 7 (Node 5 Empty Empty) (Node 10 Empty Empty))
-- *AVL> rotateL t1
-- Node 7 (Node 4 (Node 3 Empty Empty) (Node 5 Empty Empty)) (Node 10 Empty Empty)

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

-- *AVL> rotateR t1
-- Node 15 (Node 10 (Node 8 Empty Empty) Empty) Empty
-- *AVL> (rotateL . rotateR) t1 == t1
-- True
-- *AVL> (rotateR . rotateL) t1 == t1
-- True
-- *AVL> pp t5
-- --6
--   |--3
--   |  |--2
--   |  |  |--1
--   |  |  |  |-- /-
--   |  |  |  `-- /-
--   |  |  `-- /-
--   |  `--4
--   |     |-- /-
--   |     `-- /-
--   `--7
--      |-- /-
--      `-- /-
-- *AVL> pp t6
-- --3
--   |--2
--   |  |--1
--   |  |  |-- /-
--   |  |  `-- /-
--   |  `-- /-
--   `--6
--      |--4
--      |  |-- /-
--      |  `-- /-
--      `--7
--         |-- /-
--         `-- /-
-- *AVL> rotateL t6 == t5
-- True
-- *AVL> rotateR t5 == t6
-- True

t8 :: Tree Int
t8 = Node 6 (Node 3 (leaf 2) (Node 4 Empty (leaf 5))) (leaf 7)

t9 :: Tree Int
t9 = Node 4 (Node 3 (leaf 2) Empty) (Node 6 (leaf 5) (leaf 7))

-- *AVL> rotateLRight t8 == t9
-- True

t7 :: Tree Int
t7 = Node 6 (Node 3 (Node 2 Empty Empty) (Node 4 Empty (Node 5 Empty Empty))) (Node 7 Empty Empty)

{--
  Insert a new ordered value into the tree.
  Note that it preserves the Binary Search tree and the H-balanced properties of an AVL.
  Node: If an entry is already present, return directly the same tree
--}
ins :: Ord a => Tree a -> a -> Tree a
ins Empty v    = leaf v
ins n@(Node x l r) y
  | x == y     = n
  | x < y      = rebalance $ Node x l (ins r y)
  | otherwise  = rebalance $ Node x (ins l y) r

rebalance :: Ord a => Tree a -> Tree a
rebalance Empty                            = Empty
rebalance n@(Node _ Empty Empty)           = n
rebalance n@(Node _ _ Empty) | hBalanced n = n
                             | otherwise   = rotateR n
rebalance n@(Node _ Empty _) | hBalanced n = n
                             | otherwise   = rotateL n
rebalance n@(Node x
             l@(Node _ ll lr)
             r@(Node _ rl rr))
  | (hBalanced n)          = n
  | (heightFactor n == 2)  = if (hFactor ll lr) >= 0 then (rotateR n) else rotateL (Node x (rotateR l) r)
  | (heightFactor n == -2) = if (hFactor rl rr) <= 0  then (rotateL n) else rotateR (Node x l (rotateL r))

-- *AVL> pp $ build [1..10]
-- --4
--   |--2
--   |  |--1
--   |  |  |-- /-
--   |  |  `-- /-
--   |  `--3
--   |     |-- /-
--   |     `-- /-
--   `--8
--      |--6
--      |  |--5
--      |  |  |-- /-
--      |  |  `-- /-
--      |  `--7
--      |     |-- /-
--      |     `-- /-
--      `--9
--         |-- /-
--         `--10
--            |-- /-
--            `-- /-
-- *AVL> isAVL $ build [1..10]
-- True
-- *AVL> isAVL $ build [1..100]
-- True
-- *AVL> isAVL $ build [1..1000]
-- True

t10 :: Tree Int
t10 = Node 6 (Node 3 (leaf 2) (Node 4 Empty Empty)) (leaf 7)

-- *AVL> pp $ ins t10 100
-- --6
--   |--3
--   |  |--2
--   |  |  |-- /-
--   |  |  `-- /-
--   |  `--4
--   |     |-- /-
--   |     `-- /-
--   `--7
--      |-- /-
--      `--100
--         |-- /-
--         `-- /-

-- build an AVL from a list
build :: (Num a, Ord a) => [a] -> Tree a
build = foldl ins Empty

--prop_avl = (\ t -> abs (heightFactor t) <= 1)
-- adding
--main = do
-- verboseCheckWith stdArgs { maxSuccess = 1000, maxSize = 5 } prop_avl

{--
  Remove a node from the tree. Note that it preserves the AVL properties.
--}
remove :: (Ord a) => Tree a -> a -> Tree a
remove Empty _ = Empty

remove (Node x l r) y
  | x < y      = rebalance $ Node x l                (AVL.remove r y)
  | y < x      = rebalance $ Node x (AVL.remove l y) r
  | otherwise  = case deleteMax l of
    (Just z, t)  -> rebalance $ Node z t r
    (Nothing, _) -> Empty

-- *AVL> isAVL (BST.remove (ins (ins (ins (ins t1 1100) 1200) 1300) 1400) 1100)
-- False
-- *AVL> isAVL (AVL.remove (ins (ins (ins (ins t1 1100) 1200) 1300) 1400) 1100)
-- True
-- *AVL> pp $ AVL.remove (build [1..10]) 8
-- --4
--   |--2
--   |  |--1
--   |  |  |-- /-
--   |  |  `-- /-
--   |  `--3
--   |     |-- /-
--   |     `-- /-
--   `--7
--      |--6
--      |  |--5
--      |  |  |-- /-
--      |  |  `-- /-
--      |  `-- /-
--      `--9
--         |-- /-
--         `--10
--            |-- /-
--            `-- /-
-- *AVL> isAVL $ AVL.remove (build [1..10]) 8
-- True
-- *AVL> isAVL $ (AVL.remove (AVL.remove (build [1..10]) 2) 3)
-- True
-- *AVL> pp $ (AVL.remove (AVL.remove (build [1..10]) 2) 3)
-- --8
--   |--4
--   |  |--1
--   |  |  |-- /-
--   |  |  `-- /-
--   |  `--6
--   |     |--5
--   |     |  |-- /-
--   |     |  `-- /-
--   |     `--7
--   |        |-- /-
--   |        `-- /-
--   `--9
--      |-- /-
--      `--10
--         |-- /-
--         `-- /-
-- *AVL> pp $ (AVL.remove (AVL.remove (AVL.remove (build [1..10]) 2) 3) 1)
-- --8
--   |--6
--   |  |--4
--   |  |  |-- /-
--   |  |  `--5
--   |  |     |-- /-
--   |  |     `-- /-
--   |  `--7
--   |     |-- /-
--   |     `-- /-
--   `--9
--      |-- /-
--      `--10
--         |-- /-
--         `-- /-
-- *AVL> isAVL $ (AVL.remove (AVL.remove (AVL.remove (build [1..10]) 2) 3) 1)
-- True

{--
 Breadth first traversal
--}
breadth :: Tree a -> [a]
breadth t =
  reverse $ bf [t] []
  where
    bf :: [Tree a] -> [a] -> [a]
    bf [] q                  = q
    bf (Empty : ns)        q = bf ns q
    bf ((Node x l r) : ns) q = bf (ns ++ [l,r]) (x : q)

-- *AVL> pp (build [1..10])
-- --4
--   |--2
--   |  |--1
--   |  |  |-- /-
--   |  |  `-- /-
--   |  `--3
--   |     |-- /-
--   |     `-- /-
--   `--8
--      |--6
--      |  |--5
--      |  |  |-- /-
--      |  |  `-- /-
--      |  `--7
--      |     |-- /-
--      |     `-- /-
--      `--9
--         |-- /-
--         `--10
--            |-- /-
--            `-- /-
-- *AVL> breadth (build [1..10])
-- [4,2,8,1,3,6,9,5,7,10]

-- massyl
-- breadth :: [Tree a] -> [a]
-- breadth [] = []
-- breadth ts = concatMap (value []) ts ++ breadth (concatMap childs ts)

-- childs :: Tree a -> [Tree a]
-- childs Empty = []
-- childs (Node _ l r) = [l,r]

-- value :: [a] -> Tree a -> [a]
-- value acc Empty = acc
-- value xs (Node x _ _) = xs++[x]

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

mapT :: (a -> b) -> Tree a -> Tree b
mapT _ Empty = Empty
mapT f (Node x l r) = Node (f x) (mapT f l) (mapT f r)

-- *AVL> mapT show t1
-- Node "4" (Node "3" Empty Empty) (Node "7" (Node "5" Empty Empty) (Node "10" Empty Empty))

instance Functor Tree where
  fmap = mapT

-- *AVL> fmap show t1
-- Node "4" (Node "3" Empty Empty) (Node "7" (Node "5" Empty Empty) (Node "10" Empty Empty))
