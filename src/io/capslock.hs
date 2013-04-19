module Capslock where

import Control.Monad (forever)
import GHC.Unicode (toUpper)

main :: IO ()
main = forever
            (do putStr "Give me some lovin': "
                x <- getLine
                putStrLn (map toUpper x))
