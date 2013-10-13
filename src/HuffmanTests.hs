module HuffmanTests where

import Huffman

import Test.Framework
import Test.HUnit

testWeight1 = 2 ~=? weight (Leaf 'c' 2)
testWeight2 = 5 ~=? weight (Fork (Leaf 'c' 2) (Leaf 'd' 3) ['c', 'd'] 5)

testWeights = TestList ["testWeight1" ~: testWeight1,
                        "testWeight2" ~: testWeight2]

-- *HuffmanTests> runTestTT testWeights
-- Cases: 2  Tried: 2  Errors: 0  Failures: 0
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

testChars1 = "c"  ~=? chars (Leaf 'c' 2)
testChars2 = "cd" ~=? chars (Fork (Leaf 'c' 2) (Leaf 'd' 3) ['c', 'd'] 5)

testChars  = TestList ["testChars1" ~: testChars1,
                       "testChars2" ~: testChars2]

-- Full tests
tests = TestList [testWeights, testChars]
