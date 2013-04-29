module FunSet where

type Set a = a -> Bool

newEmpty :: Set a
newEmpty = \_ -> False

-- *FunSet> newEmpty []
-- False
-- *FunSet> newEmpty 1
-- False

-- add :: Eq a => Set a -> a -> Set a

-- contains ::  Set a -> a -> Bool

-- singleton :: Eq a => a ->Set a

-- union :: Set a -> Set a -> Set a

-- intersect :: Set a -> Set a -> Set a

-- diff :: Set a -> Set a -> Set a

-- filter' :: (a -> Bool) -> Set a -> Set a

-- remove :: Eq a => a -> Set a -> Set a
