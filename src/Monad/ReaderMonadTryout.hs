-- | Trying to understand by practicing on a small ReaderMonad sample


module Monad.ReaderMonadTryout where

import qualified Control.Monad.Reader as R

-- ask :: Reader r a -> a
-- asks :: (r -> a) -> Reader r a
-- local :: (r -> b) -> Reader b a -> Reader r a
-- runReader :: Reader r a -> r -> a

data MyContext = MyContext
  { foo :: String
  , bar :: Int
  } deriving (Show)

computation :: R.Reader MyContext (Maybe String)
computation = do
  n <- R.asks bar
  x <- R.asks foo
  return $ if n > 0
           then Just x
           else Nothing

ex1 :: Maybe String
ex1 = R.runReader computation $ MyContext "hello" 1

ex2 :: Maybe String
ex2 = R.runReader computation $ MyContext "haskell" 0

-- A simple implementation of the Reader monad

newtype MReader r a = MReader { runReader :: r -> a }

instance Monad (MReader r) where
  return a = MReader $ const a
  m >>= k  = MReader $ \r -> runReader (k (runReader m r)) r

ask :: MReader a a
ask = MReader id

asks :: (r -> a) -> MReader r a
asks = MReader

local :: (r -> b) -> MReader b a -> MReader r a
local f m = MReader $ runReader m . f

computation' :: MReader MyContext (Maybe String)
computation' = do
  n <- asks bar
  x <- asks foo
  return $ if n > 0
           then Just x
           else Nothing

ex1' :: Maybe String
ex1' = runReader computation' $ MyContext "hello" 1

ex2' :: Maybe String
ex2' = runReader computation' $ MyContext "haskell" 0
