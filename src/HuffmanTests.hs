module HuffmanTests where

import Huffman

import Test.Framework
import Test.HUnit

testWeight1 :: Test.HUnit.Test
testWeight1 = 2 ~=? weight (Leaf 'c' 2)

testWeight2 :: Test.HUnit.Test
testWeight2 = 5 ~=? weight (Fork (Leaf 'c' 2) (Leaf 'd' 3) ['c', 'd'] 5)

testWeights :: Test.HUnit.Test
testWeights = TestList ["testWeight1" ~: testWeight1, "testWeight2" ~: testWeight2]

testChars1 :: Test.HUnit.Test
testChars1 = "c"  ~=? chars (Leaf 'c' 2)
testChars2 :: Test.HUnit.Test
testChars2 = "cd" ~=? chars (Fork (Leaf 'c' 2) (Leaf 'd' 3) ['c', 'd'] 5)

testChars :: Test.HUnit.Test
testChars  = TestList ["testChars1" ~: testChars1, "testChars2" ~: testChars2]

testMakeCodeTree1 :: Test.HUnit.Test
testMakeCodeTree1 = Fork (Leaf 'c' 1) (Leaf 'd' 2) "cd" 3
                    ~=?
                    makeCodeTree (Leaf 'c' 1) (Leaf 'd' 2)
testMakeCodeTree2 :: Test.HUnit.Test
testMakeCodeTree2 = Fork (Fork (Leaf 'c' 1) (Leaf 'd' 2) "cd" 3) (Leaf 'd' 2) "cdd" 5
                    ~=?
                    makeCodeTree (Fork (Leaf 'c' 1) (Leaf 'd' 2) "cd" 3) (Leaf 'd' 2)

testMakeCodeTrees :: Test.HUnit.Test
testMakeCodeTrees = TestList ["testMakeCodeTree1" ~: testMakeCodeTree1, "testMakeCodeTree2" ~: testMakeCodeTree2]

testTimes1 :: Test.HUnit.Test
testTimes1 = [('1',4),('2',1),('3',1),('4',1)] ~=? times ['1','3','4','1','1','1','2']
testTimes2 :: Test.HUnit.Test
testTimes2 = [('a',4),('b',2),('d',1)] ~=? times ['a','b','b','a','a','a','d']

testTimess :: Test.HUnit.Test
testTimess = TestList ["testTimes1" ~: testTimes1, "testTimes2" ~: testTimes2]

-- *Huffman>
--
-- *Huffman>
--

testMakeOrderedLeafList1 :: Test.HUnit.Test
testMakeOrderedLeafList1 = [Leaf 'd' 3,Leaf 'b' 5,Leaf 'a' 10,Leaf 'e' 11]
                           ~=?
                           makeOrderedLeafList [('a', 10), ('b', 5), ('d', 3), ('e', 11)]

testMakeOrderedLeafList2 :: Test.HUnit.Test
testMakeOrderedLeafList2 = [Leaf 'd' 3,Leaf 'b' 5,Leaf 'a' 10,Leaf 'e' 11,Leaf ' ' 100]
                           ~=?
                           makeOrderedLeafList [(' ', 100), ('a', 10), ('b', 5), ('d', 3), ('e', 11)]

testMakeOrderedLeafLists :: Test.HUnit.Test
testMakeOrderedLeafLists = TestList ["testMakeOrderedLeafList1" ~: testMakeOrderedLeafList1,
                                     "testMakeOrderedLeafList2" ~: testMakeOrderedLeafList2]

testSingleton1 :: Test.HUnit.Test
testSingleton1 = False ~=? singleton [Leaf 'd' 3,Leaf 'b' 5,Leaf 'a' 10,Leaf 'e' 11,Leaf ' ' 100]
testSingleton2 :: Test.HUnit.Test
testSingleton2 = True ~=? singleton [Leaf 'd' 3]

testSingletons :: Test.HUnit.Test
testSingletons = TestList ["testSingleton1" ~: testSingleton1,
                           "testSingleton2" ~: testSingleton2]

testCombine1 :: Test.HUnit.Test
testCombine1 = [Leaf 'c' 15,Fork (Leaf 'a' 10) (Leaf 'b' 20) "ab" 30]
               ~=?
               combine [Leaf 'a' 10,Leaf 'b' 20, Leaf 'c' 15]
testCombine2 :: Test.HUnit.Test
testCombine2 = [Fork (Leaf 'a' 10) (Leaf 'b' 20) "ab" 30,Leaf 'c' 40]
               ~=?
               combine [Leaf 'a' 10,Leaf 'b' 20, Leaf 'c' 40]

testCombines :: Test.HUnit.Test
testCombines = TestList ["testCombine1" ~: testCombine1,
                         "testCombine2" ~: testCombine2]

testUntil1 :: Test.HUnit.Test
testUntil1 = [Fork (Leaf 'c' 20) (Fork (Leaf 'a' 10) (Leaf 'b' 20) "ab" 30) "cab" 50]
             ~=?
             Huffman.until singleton combine [Leaf 'a' 10,Leaf 'b' 20, Leaf 'c' 20]

testUntil2 :: Test.HUnit.Test
testUntil2 = [Fork (Fork (Leaf 'a' 10) (Leaf 'b' 20) "ab" 30) (Fork (Leaf 'c' 20) (Leaf 'd' 21) "cd" 41) "abcd" 71]
             ~=?
             Huffman.until singleton combine [Leaf 'a' 10,Leaf 'b' 20, Leaf 'c' 20, Leaf 'd' 21]

testUntil3 :: Test.HUnit.Test
testUntil3 = [Fork (Fork (Fork (Fork (Leaf 'n' 1) (Leaf 'r' 1) "nr" 2) (Leaf 'b' 2) "nrb" 4) (Leaf 'o' 4) "nrbo" 8) (Fork (Fork (Leaf 'e' 2) (Leaf 't' 3) "et" 5) (Leaf ' ' 5) "et " 10) "nrboet " 18]
             ~=?
             Huffman.until singleton combine [Leaf 'n' 1,Leaf 'r' 1,Leaf 'b' 2,Leaf 'e' 2,Leaf 't' 3,Leaf 'o' 4,Leaf ' ' 5]

testUntils :: Test.HUnit.Test
testUntils = TestList ["testUntil1" ~: testUntil1,
                       "testUntil2" ~: testUntil2,
                       "testUntil3" ~: testUntil3]

testCreateCodeTree1 :: Test.HUnit.Test
testCreateCodeTree1 = Fork (Fork (Fork (Fork (Leaf 'n' 1) (Leaf 'r' 1) "nr" 2) (Leaf 'b' 2) "nrb" 4) (Leaf 'o' 4) "nrbo" 8) (Fork (Fork (Leaf 'e' 2) (Leaf 't' 3) "et" 5) (Leaf ' ' 5) "et " 10) "nrboet " 18
                      ~=?
                      createCodeTree "to be or not to be"

testCreateCodeTrees :: Test.HUnit.Test
testCreateCodeTrees = TestList ["testCreateCodeTree1" ~: testCreateCodeTree1]

-- http://pl.wikipedia.org/wiki/Plik:Huffman_%28To_be_or_not_to_be%29.svg
testDecode1 :: Test.HUnit.Test
testDecode1 = "to be or not to be"
              ~=?
              decode (createCodeTree "to be or not to be") [1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 0]

testDecodes :: Test.HUnit.Test
testDecodes = TestList ["testDecode1" ~: testDecode1]

testEncode1 :: Test.HUnit.Test
testEncode1 = [1,0,1,0,1,1,1,0,0,1,1,0,0,1,1,0,1,0,0,0,1,1,1,0,0,0,0,0,1,1,0,1,1,1,1,0,1,0,1,1,1,0,0,1,1,0,0]
              ~=?
              encode (createCodeTree "to be or not to be") "to be or not to be"

testEncodes :: Test.HUnit.Test
testEncodes = TestList ["testEncode1" ~: testEncode1]

testDecodeEncode1 :: Test.HUnit.Test
testDecodeEncode1 = "to be or not to be"
                    ~=?
                    let st = "to be or not to be"
                        ct = createCodeTree st in decode ct $ encode ct st

testEncodeDecodes :: Test.HUnit.Test
testEncodeDecodes = TestList ["testEncode1" ~: testEncode1]

-- Full tests
tests :: Test.HUnit.Test
tests = TestList [testWeights,
                  testChars,
                  testMakeCodeTrees,
                  testTimess,
                  testMakeOrderedLeafLists,
                  testSingletons,
                  testCombines,
                  testUntils,
                  testCreateCodeTrees,
                  testDecodes,
                  testEncodes,
                  testEncodeDecodes]

-- *HuffmanTests> runTestTT tests
-- Cases: 21  Tried: 21  Errors: 0  Failures: 0
-- Counts {cases = 21, tried = 21, errors = 0, failures = 0}
