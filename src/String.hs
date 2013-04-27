module String where

import Data.Char (toUpper)

capitalize :: String -> String
capitalize [] = []
capitalize (h:t) = toUpper h : t
