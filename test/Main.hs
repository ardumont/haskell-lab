module Main where

import           AnagramTests          (tests)
import           BSTTests              (tests)
import qualified Data.Map              as Map
import           HuffmanTests          (tests)
import           LoadAndUpdateIniTests (tests)
import           RBTTests              (tests)
import           Test.HUnit

main :: IO ()
main = let tts = concat [AnagramTests.tests
                        ,HuffmanTests.tests
                        ,LoadAndUpdateIniTests.tests
                        ,BSTTests.tests
                        ,RBTTests.tests]
  in runTestTT (TestList tts) >>= print
