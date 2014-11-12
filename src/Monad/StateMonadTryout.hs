-- | Trying to understand by practicing on a small StateMonad sample

module Monad.StateMonadTryout where

-- runState  :: State s a -> s -> (a, s)
-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s

newtype MState s a = MState { mrunState :: s -> (a, s) }

instance Monad (MState s) where
  return a = MState $ \s -> (a, s)

  MState act >>= k = MState $ \s ->
    let (a, s') = act s
    in mrunState (k a) s'

mget :: MState s s
mget = MState $ \s -> (s, s)

mput :: s -> MState s ()
mput s = MState $ const ((), s)

mmodify :: (s -> s) -> MState s ()
mmodify f = mget >>= \x -> mput (f x)

mevalState :: MState s a -> s -> a
mevalState act = fst . mrunState act

mexecState :: MState s a -> s -> s
mexecState act = snd . mrunState act

test :: MState Int Int
test = do
  mput 3
  mmodify (+1)
  mget

main :: IO ()
main = print $ mexecState test 0
