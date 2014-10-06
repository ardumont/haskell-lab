{-# LANGUAGE RankNTypes #-}

module StateCont (
   -- | Inject pure value inside a statefull computation
   inject,
   -- | Chain stateful computations
   (>>>=),
   -- | Consult the state
   get,
   -- | Change the state
   put,
   -- | Modify the state using the given function
   modify,
   -- | Map a given function through a given state
   mapS,
   -- | State type constructor
   State)
where

newtype State s a = State { runState :: forall r. s -> (a -> s -> r) -> r }

-- | Inject pure value inside a statefull computation
inject :: a -> State s a
inject = undefined

-- | Map a given function throught a given state
mapS :: (a -> b) -> State s a -> State s b
mapS = undefined

-- | Chain statfull computations
bind ::  State s a -> (a -> State s b) -> State s b
bind =  undefined

-- | Infix operation equivalent of bind
infixr 1 >>>=
(>>>=) :: State s a -> (a -> State s b) -> State s b
(>>>=) = bind

-- | gets current state
get :: State s s
get = undefined

-- | updates current states by the give value
put :: s -> State s ()
put = undefined

-- | uplies the given function to current State
modify  :: (s -> s) -> State s ()
modify = undefined

-- | extract current State and discards the result
execState :: s -> State s a -> s
execState = undefined

-- | extracts current result and discards the State
evalState :: s -> State s a -> a
evalState = undefined

-- | making State s a an instance fo monad to make do notation availalbe
instance Monad (State s) where
 return = inject
 (>>=) = (>>>=)

-----------------------------------------------------------------------------
-- Test
-----------------------------------------------------------------------------

-- | simple function that manipulate the State, just to show how we can use it
useState ::State Int Int
useState = do
          s <- get
          let r = s*3 + 1
          put r
          modify (* 3)
          return r

funcToMap :: Show a => a -> String
funcToMap a = show a ++ "_mapped"

-- | Run computations using State S a
{- When execute main the output will be :
  4
  3
  4
using execState with get and put :
  12
using evalState with get and put :
  4
  "6_mapped"
-}
main = do
      print $ (runState $ return 1) 3 (+)
      print $ (runState $ get >>= return . (+2)) 1 const
      print $ (runState $ get >>= return . (+2)) 1 (+)
      putStrLn "using execState with get and put : "
      print $  execState 1 useState
      putStrLn "using evalState with get and put : "
      print $ evalState 1  useState
      print $ evalState 4 (mapS funcToMap (get >>= return . (+2)))
