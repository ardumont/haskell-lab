module FunSet where

type Set a = a -> Bool

newEmpty :: Set a
newEmpty = \_ -> False

-- *FunSet> newEmpty []
-- False
-- *FunSet> newEmpty 1
-- False

add :: Eq a => Set a -> a -> Set a
add s e = \ i -> (e == i) || s i

-- *FunSet> ((add newEmpty 1) 1)
-- True
-- *FunSet> ((add newEmpty 1) 2)
-- False

contains :: Set a -> a -> Bool
contains s e = s e

-- *FunSet> contains newEmpty 1
-- False
-- *FunSet> contains (add newEmpty 1) 1
-- True
-- *FunSet> contains (add newEmpty 1) 2
-- False

singleton :: Eq a => a -> Set a
singleton a = \ e -> (a == e)

-- *FunSet> contains (singleton 1) 1
-- True
-- *FunSet> contains (singleton 1) 3
-- False

union :: Set a -> Set a -> Set a
union a b = \ e -> a e || b e

-- *FunSet> (union (singleton 1) (singleton 2)) 1
-- True
-- *FunSet> (union (singleton 1) (singleton 2)) 2
-- True
-- *FunSet> (union (singleton 1) (singleton 2)) 3
-- False

intersect :: Set a -> Set a -> Set a
intersect a b = \ e -> a e && b e

-- *FunSet> (intersect (union (singleton 1) (singleton 2)) (singleton 1)) 2
-- False
-- *FunSet> (intersect (union (singleton 1) (singleton 2)) (singleton 1)) 1
-- True

diff :: Set a -> Set a -> Set a
diff a b = \ e -> a e && not (b e)

-- *FunSet> (diff (union (singleton 1) (singleton 2)) (union (singleton 1) (singleton 3))) 1
-- False
-- *FunSet> (diff (union (singleton 1) (singleton 2)) (union (singleton 1) (singleton 3))) 2
-- True
-- *FunSet> (diff (union (singleton 1) (singleton 2)) (union (singleton 1) (singleton 3))) 3
-- False

-- filter' :: (a -> Bool) -> Set a -> Set a

-- remove :: Eq a => a -> Set a -> Set a
