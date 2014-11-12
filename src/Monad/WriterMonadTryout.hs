-- | Trying to understand by practicing on a small ReaderMonad sample

module Monad.WriterMonadTryout where

-- tell :: w -> Writer w ()
-- execWriter :: Writer w a -> w
-- runWriter :: Writer w a -> (a, w)

import           Data.Monoid

-- Definition

newtype MWriter w a = MWriter { runMWriter :: (a, w) }

instance Monoid w => Monad (MWriter w) where
  return a = MWriter (a, mempty)
  m >>= k  = MWriter $ let
      (a, w)  = runMWriter m
      (b, w') = runMWriter (k a)
      in (b, w `mappend` w')

execMWriter :: MWriter w a -> w
execMWriter m = snd (runMWriter m)

mtell :: w -> MWriter w ()
mtell w = MWriter ((), w)

-- Sample

type MyMWriter = MWriter [Int] String

example :: MyMWriter
example  = do
  mtell [1..5]
  mtell [5..10]
  return "foo"

output :: (String, [Int])
output = runMWriter example
