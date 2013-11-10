module BinarySearchTree where

import Data.List (foldl')

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
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show)

-- Example of Binary Search Trees that may be used to test your implementation

leaf :: a -> Tree a
leaf x = Node x Empty Empty

pp :: Show a => Tree a -> IO ()
pp = (mapM_ putStrLn) . treeIndent
  where
    treeIndent Empty          = ["-- /-"]
    treeIndent (Node v lb rb) =
      ["-- " ++ (show v)] ++
      map ("  |" ++) ls ++
      ("  `" ++ r) : map ("   " ++) rs
      where
        (r:rs) = treeIndent $ rb
        ls     = treeIndent $ lb

-- The size of the tree is taken to be the number n of internal nodes
--(those with two children)
size :: Num a => Tree b -> a
size Empty        = 0
size (Node _ l r) = 1 + size l + size r

-- Returns an unsorted list of all values in the given Tree
-- (we need to be able to rebuild the tree from the list)
toList :: Tree a -> [a]
toList Empty        = []
toList (Node x l r) = x : (toList l) ++ (toList r)

fromList :: Ord a => [a] -> Tree a
fromList = foldl' insert Empty

-- Returns a sorted list of all elements of the given Tree.
-- Note that we can't go back to the origin Tree
toSortedList :: Tree a -> [a]
toSortedList Empty        = []
toSortedList (Node x l r) = toSortedList l ++ x : toSortedList r

-- Returns the smallest value in the given Tree
smallValue :: Tree a ->  Maybe a
smallValue Empty            = Nothing
smallValue (Node x Empty _) = Just x
smallValue (Node _ l _)     = smallValue l

-- Returns the greatest value in the the given Tree
greatValue :: Tree a -> Maybe a
greatValue Empty            = Nothing
greatValue (Node x _ Empty) = Just x
greatValue (Node _ _ r)     = greatValue r

{-- Returns The mirror of the given Tree. for example:
  t2 = Node 20 (Node 15 (Node 8 (leaf 7) (leaf 11)) (leaf 18))
               (Node 118
                       (Node 35 (leaf 33) (Node 49 Empty (leaf 60)))
                       (leaf 166))

 mirror t2 must return :
  Node 17 (Node 115 (Node 163 Empty Empty)
                    (Node 32 (Node 46 (Node 57 Empty Empty)
                                       Empty)
                             (Node 30 Empty Empty)))
          (Node 12 (Node 15 Empty Empty)
                   (Node 5 (Node 8 Empty Empty) Empty))
--}
mirror :: Tree a -> Tree a
mirror Empty        = Empty
mirror (Node x l r) = Node x (mirror r) (mirror l)

-- Returns whether the given Tree contains the given element or not
contains :: Ord a => Tree a -> a -> Bool
contains Empty _        = False
contains (Node x l r) y = case compare y x of
  EQ -> True
  LT -> contains l y
  GT -> contains r y

-- Returns the right son of the given Tree
rightSon :: Tree a -> Tree a
rightSon Empty        = Empty
rightSon (Node _ _ r) = r

-- Returns the left son of the given Tree
leftSon :: Tree a -> Tree a
leftSon Empty        = Empty
leftSon (Node _ l _) = l

{--
  Insert a new ordered value into the tree.
  Note that it preserves the Binary Search Tree properties,
  but not the balanced properties of an AVL for example.
  For the first implementation do not bother yourself about the
  balanced properties.
--}

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty x = leaf x
insert n@(Node x l r) y = case compare y x of
  EQ -> n
  GT -> Node x l (insert r y)
  _  -> Node x (insert l y) r

value :: Tree a -> Maybe a
value Empty        = Nothing
value (Node x _ _) = Just x

isBSearchTree :: (Ord a) => Tree a -> Bool
isBSearchTree Empty = True
isBSearchTree (Node x l r) =
  case [value l, value r] of
    [Nothing, Nothing] -> True
    [Nothing, Just z]  -> and [x < z, isBSearchTree l, isBSearchTree r]
    [Just y, Nothing]  -> and [y <= x, isBSearchTree l, isBSearchTree r]
    [Just y, Just z]   -> and [y <= x, x < z, isBSearchTree l, isBSearchTree r]

deleteMax :: Tree a -> (Maybe a, Tree a)
deleteMax Empty            = (Nothing, Empty)
deleteMax (Node x _ Empty) = (Just x, Empty)
deleteMax (Node x l r)     = let (y, t) = deleteMax r in
                             (y, (Node x l t))

deleteMin :: Tree a -> (Maybe a, Tree a)
deleteMin Empty            = (Nothing, Empty)
deleteMin (Node x Empty _) = (Just x, Empty)
deleteMin (Node x l r)     = let (y, t) = deleteMin l in
                             (y, (Node x t r))

-- Remove an element from a tree.
-- To remove a node, take the max element from the left tree and replace the node to be
-- removed with this one
remove :: Ord a => Tree a -> a -> Tree a
remove Empty _  = Empty
remove (Node x l r) y
  | y < x     = Node x (remove l y) r
  | y > x     = Node x l (remove r y)
  | otherwise = case deleteMax l of
    (Just z, t)  -> Node z t r
    (Nothing, _) -> Empty
