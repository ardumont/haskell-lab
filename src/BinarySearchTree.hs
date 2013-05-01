module BinarySearchTree where

{--
Given the following binary Tree data type, provide an implementation of all functions
defined in this file.
Note that this definition is totaly lazy and that there is no constraints defined on elements
that will be associated with each Node.

Three may be declared as : data (Ord a) => Tree a = Empty | Node a (Tree a) (Tree a)
but it is not the right option.

To enforce the property of Binary Search Trees, functions that insert elements in the
given Tree must be implemeted in such a way that the invariant of a binary search tree always hold.
--}
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq,Show)

-- Helper funciton
leaf :: a -> Tree a
leaf x = Node x Empty Empty

-- Example of Binary Search Trees that may be used to test your implementation

t1 :: Tree Int
t1 = Node 4 (leaf 3) (Node 7 (leaf 5) (leaf 10))

t2 :: Tree Int
t2 = Node 20 (Node 15 (Node 8 (leaf 7) (leaf 11)) (leaf 18))
             (Node 118
                     (Node 35 (leaf 33) (Node 49 Empty (leaf 60)))
                     (leaf 166))

-- The size of the tree is taken to be the number n of internal nodes
--(those with two children)
size :: Num a => Tree b -> a
size Empty = 0
size (Node _ l r) = 1 + size l + size r

-- *BinarySearchTree> size t1
-- 5
-- *BinarySearchTree> size t2
-- 12

-- Returns an unsorted list of all values in the given Tree
-- (we need to be able to rebuild the tree from the list)
toList :: Tree a -> [a]
toList Empty = []
toList (Node x l r) = [x] ++ (toList l) ++ (toList r)

-- *BinarySearchTree> toList t1
-- [4,3,7,5,10]
-- *BinarySearchTree> toList t2
-- [20,15,8,7,11,18,118,35,33,49,60,166]

fromList :: Ord a => [a] -> Tree a
fromList [] = Empty
fromList (x:xs) = Node x (fromList lefts) (fromList rights)
                  where p      = (<= x)
                        lefts  = takeWhile p xs
                        rights = dropWhile p xs

-- *BinarySearchTree> (fromList . toList)  t1 == t1
-- True
-- *BinarySearchTree> (fromList . toList)  t1 == (leaf 1)
-- False
-- *BinarySearchTree> (fromList . toList) t2 == t2
-- True
-- *BinarySearchTree> (fromList . toList) t2 == (leaf 1)
-- False

-- Returns a sorted list of all elements of the given Tree.
-- Note that we can't go back to the origin Tree
toSortedList :: Tree a -> [a]
toSortedList Empty = []
toSortedList (Node x l r) = toSortedList l ++ [x] ++ toSortedList r

-- *BinarySearchTree> toSortedList t1
-- [3,4,5,7,10]
-- *BinarySearchTree> toSortedList t2
-- [7,8,11,15,18,20,33,35,49,60,118,166]

-- Returns the smallest value in the given Tree
smallValue :: Tree a -> Maybe a
smallValue Empty            = Nothing
smallValue (Node x Empty _) = Just x
smallValue (Node _ l _)     = smallValue l

-- *BinarySearchTree> smallValue t1 == Just (head (toSortedList t1))
-- True
-- *BinarySearchTree> smallValue t2 == Just (head (toSortedList t2))
-- True
-- *BinarySearchTree> smallValue Empty == Nothing
-- True

-- Returns the greatest value in the the given Tree
greatValue :: Tree a -> a
greatValue  = undefined

{-- Returns The mirror of the given Tree. for example
 mirror t2 must return :
  Node 17 (Node 115 (Node 163 Empty Empty)
                    (Node 32 (Node 46 (Node 57 Empty Empty)
                                       Empty)
                             (Node 30 Empty Empty)))
          (Node 12 (Node 15 Empty Empty)
                   (Node 5 (Node 8 Empty Empty) Empty))
--}
mirror :: Tree a -> Tree a
mirror = undefined

-- Returns whether the given Tree is empty or not
empty :: Tree a -> Bool
empty = undefined

-- Returns whether the given Tree contains the given element or not
contains :: Ord a =>  Tree a -> a -> Bool
contains = undefined

-- Returns the right son of the given Tree
rightSon :: Tree a -> Tree a
rightSon  = undefined

-- Returns the left son of the given Tree
leftSon :: Tree a -> Tree a
leftSon = undefined

{--
  Insert an new ordred value into the tree.
  Note that it preserves the Binary Search Tree propertie,
  but not the balanced propertie of an AVL for example.
  for the first implementation do not bother yourself about the
  balanced propertie.
--}

insert :: (Ord a) => Tree a -> a -> Tree a
insert = undefined

isBSearchTree :: (Ord a) => Tree a -> Bool
isBSearchTree  = undefined
