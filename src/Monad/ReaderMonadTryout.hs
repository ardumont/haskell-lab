-- | Trying to understand by practicing on a small ReaderMonad sample

module Monad.ReaderMonadTryout where

-- ask :: Reader r a -> a
-- asks :: (r -> a) -> Reader r a
-- local :: (r -> b) -> Reader b a -> Reader r a
-- runReader :: Reader r a -> r -> a

newtype MReader r a = MReader { rrunReader :: r -> a }

instance Monad (MReader r) where
  return a = MReader $ const a
  m >>= k  = MReader $ \r -> rrunReader (k (rrunReader m r)) r

ask :: MReader a a
ask = MReader id

rasks :: (r -> a) -> MReader r a
rasks = MReader

local :: (r -> b) -> MReader b a -> MReader r a
local f m = MReader $ rrunReader m . f

data MyContext = MyContext
  { foo :: String
  , bar :: Int
  } deriving (Show)

rcomputation :: MReader MyContext (Maybe String)
rcomputation = do
  n <- rasks bar
  x <- rasks foo
  return $ if n > 0
           then Just x
           else Nothing

ex1 :: Maybe String
ex1 = rrunReader rcomputation $ MyContext "hello" 1

ex2 :: Maybe String
ex2 = rrunReader rcomputation $ MyContext "haskell" 0
