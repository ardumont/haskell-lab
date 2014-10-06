-- | Understanding the article about profunctor
-- source: https://www.fpcomplete.com/user/liyang/profunctors

module Profunctors where

-- import           Data.Functor
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- import Data.Functor.Contravariant

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

-- type Predicate a = a -> Bool

newtype Predicate a = Predicate { getPredicate :: a -> Bool }

instance Contravariant Predicate where
  contramap g (Predicate p) = Predicate (p . g)

veryOdd :: Predicate Integer
veryOdd = contramap (`div` 2) (Predicate odd)

main :: IO ()
main = print $ getPredicate veryOdd <$> [0..11]
