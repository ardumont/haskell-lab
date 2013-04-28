module Monade where

data May a = Noth | Jus a

instance Monad May where
         return = Jus

         Noth  >>= _ = Noth
         (Jus x) >>= f = f x

instance Monad [] where
         return v = [v]

         -- m a -> (a -> m b) -> m b
         -- [a] -> (a -> [b]) -> [b]
         l >>= f = concatMap f l
