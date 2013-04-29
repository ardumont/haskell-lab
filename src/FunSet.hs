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


-- singleton :: Eq a => a ->Set a

-- union :: Set a -> Set a -> Set a

-- intersect :: Set a -> Set a -> Set a

-- diff :: Set a -> Set a -> Set a

-- filter' :: (a -> Bool) -> Set a -> Set a

-- remove :: Eq a => a -> Set a -> Set a
