module Capslock where

import GHC.Unicode (toUpper)

main :: IO ()
main = do x <- getContents
          putStrLn (map toUpper x)

-- getContents :: IO String

-- cat resources/quotes | runhaskell capslock
-- I THINK PROGRAMMERS HAVE BECOME
-- INURED TO INCIDENTAL COMPLEXITY...
-- WHEN THEY ENCOUNTER COMPLEXITY,
-- THEY CONSIDER IT A CHALLENGE TO OVERCOME,
-- RATHER THAN AN OBSTACLE TO REMOVE.
-- OVERCOMING COMPLEXITY ISN'T WORK, IT'S WASTE.
