-- | Trying to understand by practicing on a small ReaderMonad sample

module Monad.WriterMonadTryout where

-- tell :: w -> Writer w ()
-- execWriter :: Writer w a -> w
-- runWriter :: Writer w a -> (a, w)

import           Control.Monad.Writer

type MyWriter = Writer [Int] String

example :: MyWriter
example  = do
  tell [1..5]
  tell [5..10]
  return "foo"

output :: (String, [Int])
output = runWriter example
