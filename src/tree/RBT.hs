module RBT where

import Data.List (foldl', sort, nub)

--import Test.QuickCheck
import Test.HUnit
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- import Test.Framework.Options (TestOptions, TestOptions'(..))
-- import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
-- import Test.Framework.Providers.HUnit

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

-- FIXME create your own type to permit the behaviour sharing between RBT and BST as a RBT is a BST
-- this function is repeated and adapted from the BST module, this is not DRY!

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
paths = undefined

testNoRedRed1 :: Test
testNoRedRed1 = True  ~=? noRedRed t where t = Node B (Node R (Node B Empty 5 Empty) 10 Empty) 20 Empty :: Tree Int

testNoRedRed2 :: Test
testNoRedRed2 = False ~=? noRedRed t where t = Node R (Node R (Node B Empty 5 Empty) 10 Empty) 20 Empty :: Tree Int

testNoRedRed3 :: Test
testNoRedRed3 = False ~=? noRedRed t where t = Node B (Node R (Node R Empty 5 Empty) 10 Empty) 20 Empty :: Tree Int

testNoRedRed4 :: Test
testNoRedRed4 = True  ~=? noRedRed t where t = Node B (Node R (Node B Empty 5 Empty) 10 Empty) 20 Empty :: Tree Int

testNoRedReds :: Test
testNoRedReds = TestList [testNoRedRed1, testNoRedRed2, testNoRedRed3, testNoRedRed4]

testCountRB1 :: Test
testCountRB1 = (2,4) ~=? countRB t where t = Node B (Node R (Node B Empty 0 Empty) 1 (Node B (Node R Empty 2 Empty) 3 Empty)) 4 (Node B Empty 5 Empty) :: Tree Int

testCountRB2 :: Test
testCountRB2 = (4,3) ~=? countRB t where t = Node B (Node B (Node R Empty 1 Empty) 3 (Node R Empty 2 Empty)) 4 (Node B (Node R Empty 5 Empty) 6 (Node R Empty 7 Empty)) :: Tree Int

testCountRB3 :: Test
testCountRB3 = (3,4) ~=? countRB t where t = Node B (Node B Empty 0 Empty) 1 (Node R (Node B (Node R Empty 3 Empty) 4 (Node R Empty 5 Empty)) 6 (Node B Empty 7 Empty)) :: Tree Int

testCountRBs :: Test
testCountRBs = TestList [testCountRB1, testCountRB2, testCountRB3]

prop_sort_list_2_RBT_to_sorted_list :: [Int] -> Bool
prop_sort_list_2_RBT_to_sorted_list xs =
  sortedResult == expectedSortedList
  where sortedResult = (toSortedList . fromList) xs
        expectedSortedList = sort sortedResult

prop_rbt_is_a_bst :: [Int] -> Bool
prop_rbt_is_a_bst xs = (isBST . fromList) xs == True

prop_rbt :: [Int] -> Bool
prop_rbt xs = (isRBT . fromList) xs == True

prop_insert_element_is_contained_in_tree :: [Int] -> Int -> Bool
prop_insert_element_is_contained_in_tree xs e =
  (contains . fromList) xs e == elem e xs

prop_fromList_build_a_tree :: [Int] -> Bool
prop_fromList_build_a_tree xs = (length . toList . fromList) xs == (length . nub) xs

-- deepCheck :: Testable prop => prop -> IO ()
-- deepCheck p = quickCheckWith stdArgs { maxSuccess = 500} p

testsQuick = [
  testGroup "Group of tests" [
     testProperty "A (R)ed (B)lack (T)ree should be a (B)inary (S)earch (T)ree" prop_rbt_is_a_bst,
     testProperty "fromList should always create a RBT" prop_rbt,
     testProperty "Sorted list from a RBT Should return a sorted list" prop_sort_list_2_RBT_to_sorted_list,
     testProperty "Element inserted is contained in the RBT" prop_insert_element_is_contained_in_tree,
     testProperty "Length of the list built from the RBT" prop_fromList_build_a_tree
     ]
  ]

testsHUnit :: Test
testsHUnit = TestList [testCountRBs,
                       testNoRedReds]

main :: IO ()
main = runTestTT testsHUnit >> defaultMain testsQuick
