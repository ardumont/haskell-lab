module Main where

import Control.Monad (when, forever)
import GHC.Unicode (toUpper)

main :: IO ()
main = do putStrLn "Hello, what's your name?"
          n <- getLine
          putStrLn $ "Hello " ++ reverse n

infiniteRead :: IO ()
infiniteRead = do line <- getLine
                  if null line
                    then return ()
                    else do putStrLn $ reverseWords line
                            infiniteRead

reverseWords :: String -> String
reverseWords = unwords . reverse . words

-- *Main> reverseWords "this is to be reversed"
-- "reversed be to is this"

readL :: IO ()
readL =
  do x <- getChar
     when (x /= '\n')
       (do putChar x
           readL)

-- some small sequencing using do

sequenceRead :: IO ()
sequenceRead = do rs <- sequence [getLine, getLine, getLine]
                  print rs

-- *Main> sequence (map print [1,2,3])
-- 1
-- 2
-- 3
-- [(),(),()]

-- *Main> :t mapM
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- *Main> mapM print [1,2,3]
-- 1
-- 2
-- 3
-- [(),(),()]

-- *Main> mapM_ print [1,2,3]
-- 1
-- 2
-- 3
-- *main> :t mapM_
-- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()

readOnce :: IO ()
readOnce = forever
           (do putStr "Give me some lovin': "
               x <- getLine
               putStrLn (map toUpper x))

-- forever :: Monad m => m a -> m b
