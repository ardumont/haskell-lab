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
