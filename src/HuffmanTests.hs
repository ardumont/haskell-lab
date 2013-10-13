module HuffmanTests where

import Huffman

import Test.Framework
import Test.HUnit

testWeight1 = 2 ~=? weight (Leaf 'c' 2)
testWeight2 = 5 ~=? weight (Fork (Leaf 'c' 2) (Leaf 'd' 3) ['c', 'd'] 5)

testWeights = TestList ["testWeight1" ~: testWeight1, "testWeight2" ~: testWeight2]

testChars1 = "c"  ~=? chars (Leaf 'c' 2)
testChars2 = "cd" ~=? chars (Fork (Leaf 'c' 2) (Leaf 'd' 3) ['c', 'd'] 5)

testChars  = TestList ["testChars1" ~: testChars1, "testChars2" ~: testChars2]

testMakeCodeTree1 = Fork (Leaf 'c' 1) (Leaf 'd' 2) "cd" 3
                    ~=?
                    makeCodeTree (Leaf 'c' 1) (Leaf 'd' 2)
testMakeCodeTree2 = Fork (Fork (Leaf 'c' 1) (Leaf 'd' 2) "cd" 3) (Leaf 'd' 2) "cdd" 5
                    ~=?
                    makeCodeTree (Fork (Leaf 'c' 1) (Leaf 'd' 2) "cd" 3) (Leaf 'd' 2)

testMakeCodeTrees = TestList ["testMakeCodeTree1" ~: testMakeCodeTree1, "testMakeCodeTree2" ~: testMakeCodeTree2]

testTimes1 = [('1',4),('2',1),('3',1),('4',1)] ~=? times ['1','3','4','1','1','1','2']
testTimes2 = [('a',4),('b',2),('d',1)] ~=? times ['a','b','b','a','a','a','d']

testTimess = TestList ["testTimes1" ~: testTimes1, "testTimes2" ~: testTimes2]

-- *Huffman>
--
-- *Huffman>
--

testMakeOrderedLeafList1 = [Leaf 'd' 3,Leaf 'b' 5,Leaf 'a' 10,Leaf 'e' 11]
                           ~=?
                           makeOrderedLeafList [('a', 10), ('b', 5), ('d', 3), ('e', 11)]

testMakeOrderedLeafList2 = [Leaf 'd' 3,Leaf 'b' 5,Leaf 'a' 10,Leaf 'e' 11,Leaf ' ' 100]
                           ~=?
                           makeOrderedLeafList [(' ', 100), ('a', 10), ('b', 5), ('d', 3), ('e', 11)]

testMakeOrderedLeafLists = TestList ["testMakeOrderedLeafList1" ~: testMakeOrderedLeafList1,
                                     "testMakeOrderedLeafList2" ~: testMakeOrderedLeafList2]

testSingleton1 = False ~=? singleton [Leaf 'd' 3,Leaf 'b' 5,Leaf 'a' 10,Leaf 'e' 11,Leaf ' ' 100]
testSingleton2 = True ~=? singleton [Leaf 'd' 3]

testSingletons = TestList ["testSingleton1" ~: testSingleton1,
                           "testSingleton2" ~: testSingleton2]

testCombine1 = [Leaf 'c' 15,Fork (Leaf 'a' 10) (Leaf 'b' 20) "ab" 30]
               ~=?
               combine [Leaf 'a' 10,Leaf 'b' 20, Leaf 'c' 15]
testCombine2 = [Fork (Leaf 'a' 10) (Leaf 'b' 20) "ab" 30,Leaf 'c' 40]
               ~=?
               combine [Leaf 'a' 10,Leaf 'b' 20, Leaf 'c' 40]

testCombines = TestList ["testCombine1" ~: testCombine1,
                         "testCombine2" ~: testCombine2]

testUntil1 = [Fork (Leaf 'c' 20) (Fork (Leaf 'a' 10) (Leaf 'b' 20) "ab" 30) "cab" 50]
             ~=?
             Huffman.until singleton combine [Leaf 'a' 10,Leaf 'b' 20, Leaf 'c' 20]

testUntil2 = [Fork (Fork (Leaf 'a' 10) (Leaf 'b' 20) "ab" 30) (Fork (Leaf 'c' 20) (Leaf 'd' 21) "cd" 41) "abcd" 71]
             ~=?
             Huffman.until singleton combine [Leaf 'a' 10,Leaf 'b' 20, Leaf 'c' 20, Leaf 'd' 21]

testUntils = TestList["testUntil1" ~: testUntil1,
                      "testUntil2" ~: testUntil2]

-- Full tests
tests = TestList [testWeights,
                  testChars,
                  testMakeCodeTrees,
                  testTimess,
                  testMakeOrderedLeafLists,
                  testSingletons,
                  testCombines,
                  testUntils]

-- *HuffmanTests> runTestTT tests
-- Cases: 16  Tried: 16  Errors: 0  Failures: 0
-- Counts {cases = 16, tried = 16, errors = 0, failures = 0}
