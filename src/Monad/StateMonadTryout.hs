-- | Trying to understand by practicing on a small ReaderMonad sample

module Monad.StateMonadTryout where

-- runState  :: State s a -> s -> (a, s)
-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s

import           Control.Monad.State

test :: State Int Int
test = do
  put 3
  modify (+1)
  get

main :: IO ()
main = print $ execState test 0
