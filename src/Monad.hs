module Monade where

data May a = Noth | Jus a

instance Monad May where
         return = Jus

         Noth  >>= _ = Noth
         (Jus x) >>= f = f x
