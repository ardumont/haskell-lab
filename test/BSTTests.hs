module BSTTests where

import           Tree.BinarySearchTree

import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit

testLeaf1 :: Test
testLeaf1 = leaf 1 ~=? t where t = Node 1 Empty Empty :: Tree Int

testLeaf2 :: Test
testLeaf2 = leaf 2 ~=? t where t = Node 2 Empty Empty :: Tree Int

testLeafs :: Test
testLeafs = TestList ["testLeaf1" ~: testLeaf1,
                      "testLeaf2" ~: testLeaf2]

testSize1 :: Test
testSize1 = 5 ~=? size (Node 4 (leaf 3) (Node 7 (leaf 5) (leaf 10)))

testSize2 :: Test
testSize2 = 12 ~=? size (Node 20
                         (Node 15 (Node 8 (leaf 7) (leaf 11)) (leaf 18))
                         (Node 118
                          (Node 35 (leaf 33) (Node 49 Empty (leaf 60)))
                          (leaf 166)))

testSizes :: Test
testSizes = TestList ["testSize1" ~: testSize1, "testSize2" ~: testSize2]

t1 :: Tree Int
t1 = Node 4 (leaf 3) (Node 7 (leaf 5) (leaf 10))

t2 :: Tree Int
t2 = Node 20 (Node 15 (Node 8 (leaf 7) (leaf 11)) (leaf 18))
             (Node 118
                     (Node 35 (leaf 33) (Node 49 Empty (leaf 60)))
                     (leaf 166))

testToList1 :: Test
testToList1 = [4,3,7,5,10] ~=? toList t1

testToList2 :: Test
testToList2 = [20,15,8,7,11,18,118,35,33,49,60,166] ~=? toList t2

testToLists :: Test
testToLists = TestList ["testToList1" ~: testToList1, "testToList2" ~: testToList2]

testFromListToList1 :: Test
testFromListToList1 = True ~=? (fromList . toList) t1 == t1

testFromListToList2 :: Test
testFromListToList2 = False ~=? (fromList . toList) t1 == leaf 1

testFromListToList3 :: Test
testFromListToList3 = True ~=? (fromList . toList) t2 == t2

testFromListToList4 :: Test
testFromListToList4 = False ~=? (fromList . toList) t2 == leaf 1

testFromListToLists :: Test
testFromListToLists = TestList ["testFromListToList1" ~: testFromListToList1,
                                "testFromListToList2" ~: testFromListToList2,
                                "testFromListToList3" ~: testFromListToList3,
                                "testFromListToList4" ~: testFromListToList4]

testToSortedList1 :: Test
testToSortedList1 = [3,4,5,7,10] ~=? toSortedList t1

testToSortedList2 :: Test
testToSortedList2 = [7,8,11,15,18,20,33,35,49,60,118,166] ~=? toSortedList t2

testToSortedLists :: Test
testToSortedLists = TestList ["testToSortedList1" ~: testToSortedList1,
                              "testToSortedList2" ~: testToSortedList2]

testSmallValue1 :: Test
testSmallValue1 = True ~=? smallValue t1 == Just (head (toSortedList t1))

testSmallValue2 :: Test
testSmallValue2 = True ~=? smallValue t2 == Just (head (toSortedList t2))

testSmallValue3 :: Test
testSmallValue3 = Nothing ~=? smallValue (Empty :: Tree Int)

testSmallValues :: Test
testSmallValues = TestList["testSmallValue1" ~: testSmallValue1,
                           "testSmallValue2" ~: testSmallValue2,
                           "testSmallValue3" ~: testSmallValue3]

testGreatValue1 :: Test
testGreatValue1 = True ~=? greatValue t2 == Just (last (toSortedList t2))

testGreatValue2 :: Test
testGreatValue2 = Nothing ~=? greatValue (Empty :: Tree Int)

testGreatValue3 :: Test
testGreatValue3 = True ~=? greatValue t1 == Just (last (toSortedList t1))

testGreatValues :: Test
testGreatValues = TestList["testGreatValue1" ~: testGreatValue1,
                           "testGreatValue2" ~: testGreatValue2,
                           "testGreatValue3" ~: testGreatValue3]

testMirror1 :: Test
testMirror1 = Node 4 (Node 3 Empty Empty) (Node 7 (Node 5 Empty Empty) (Node 10 Empty Empty)) ~=? t1

testMirror2 :: Test
testMirror2 = Node 4 (Node 7 (Node 10 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty) ~=? mirror t1

testMirror3 :: Test
testMirror3 = Node 20 (Node 15 (Node 8 (Node 7 Empty Empty) (Node 11 Empty Empty)) (Node 18 Empty Empty)) (Node 118 (Node 35 (Node 33 Empty Empty) (Node 49 Empty (Node 60 Empty Empty))) (Node 166 Empty Empty)) ~=? t2

testMirror4 :: Test
testMirror4 = Node 20 (Node 118 (Node 166 Empty Empty) (Node 35 (Node 49 (Node 60 Empty Empty) Empty) (Node 33 Empty Empty))) (Node 15 (Node 18 Empty Empty) (Node 8 (Node 11 Empty Empty) (Node 7 Empty Empty))) ~=? mirror t2

testMirrors :: Test
testMirrors = TestList["testMirror1" ~: testMirror1,
                       "testMirror2" ~: testMirror2,
                       "testMirror3" ~: testMirror3,
                       "testMirror4" ~: testMirror4]

testContains1 :: Test
testContains1 = True ~=? contains t1 4

testContains2 :: Test
testContains2 = True ~=? contains t1 7

testContains3 :: Test
testContains3 = True ~=? contains t1 5

testContains4 :: Test
testContains4 = True ~=? contains t1 10

testContains5 :: Test
testContains5 = False ~=? contains t1 11

testContains6 :: Test
testContains6 = False ~=? contains t1 1

testContains7 :: Test
testContains7 = True ~=? contains t1 3

testContainss :: Test
testContainss = TestList["testContains1" ~: testContains1,
                         "testContains2" ~: testContains2,
                         "testContains3" ~: testContains3,
                         "testContains4" ~: testContains4,
                         "testContains5" ~: testContains5,
                         "testContains6" ~: testContains6,
                         "testContains7" ~: testContains7]

testRightSon1 :: Test
testRightSon1 = Node 4 (leaf 3) (Node 7 (leaf 5) (leaf 10)) ~=? t1

testRightSon2 :: Test
testRightSon2 = Node 7 (leaf 5) (leaf 10) ~=? rightSon t1

testRightSon3 :: Test
testRightSon3 = Node 20 (Node 15 (Node 8 (Node 7 Empty Empty) (Node 11 Empty Empty)) (Node 18 Empty Empty)) (Node 118 (Node 35 (Node 33 Empty Empty) (Node 49 Empty (Node 60 Empty Empty))) (Node 166 Empty Empty))
                ~=?
                t2

testRightSon4 :: Test
testRightSon4 = Node 118 (Node 35 (Node 33 Empty Empty) (Node 49 Empty (Node 60 Empty Empty))) (Node 166 Empty Empty)
                ~=?
                rightSon t2

testRightSons :: Test
testRightSons = TestList["testRightSon1" ~: testRightSon1,
                         "testRightSon2" ~: testRightSon2,
                         "testRightSon3" ~: testRightSon3,
                         "testRightSon4" ~: testRightSon4]

testInsert1 :: Test
testInsert1 = Node 4 (Node 3 Empty Empty) (Node 7 (Node 5 Empty Empty) (Node 10 Empty Empty)) ~=? insert t1 10

testInsert2 :: Test
testInsert2 = Node 20 (Node 15 (Node 8 (Node 7 Empty Empty) (Node 11 Empty Empty)) (Node 18 Empty Empty)) (Node 118 (Node 35 (Node 33 Empty Empty) (Node 49 Empty (Node 60 Empty Empty))) (Node 166 Empty (Node 200 Empty Empty)))
              ~=?
              insert t2 200

testInserts :: Test
testInserts = TestList["testInsert1" ~: testInsert1,
                       "testInsert2" ~: testInsert2]

testValue1 :: Test
testValue1 = Just 10 ~=? value (Node 10 Empty Empty :: Tree Int)

testValue2 :: Test
testValue2 = Just 10 ~=? value (leaf 10 :: Tree Int)

testValue3 :: Test
testValue3 = Nothing ~=? value (Empty :: Tree Int)

testValues :: Test
testValues = TestList["testValue1" ~: testValue1,
                      "testValue2" ~: testValue2,
                      "testValue3" ~: testValue3]

testIsBinarySearchTree1 :: Test
testIsBinarySearchTree1 = False ~=? isBSearchTree (Node 10 t2 t1)

testIsBinarySearchTree2 :: Test
testIsBinarySearchTree2 = True ~=? isBSearchTree t1

testIsBinarySearchTree3 :: Test
testIsBinarySearchTree3 = True ~=? isBSearchTree t2

testIsBinarySearchTree4 :: Test
testIsBinarySearchTree4 = True ~=? isBSearchTree (insert t2 1)

testIsBinarySearchTree5 :: Test
testIsBinarySearchTree5 = True ~=? isBSearchTree (insert (insert t2 1) 100)

testIsBinarySearchTrees :: Test
testIsBinarySearchTrees = TestList["testIsBinarySearchTree1" ~: testIsBinarySearchTree1,
                                   "testIsBinarySearchTree2" ~: testIsBinarySearchTree2,
                                   "testIsBinarySearchTree3" ~: testIsBinarySearchTree3,
                                   "testIsBinarySearchTree4" ~: testIsBinarySearchTree4,
                                   "testIsBinarySearchTree5" ~: testIsBinarySearchTree5]

testDeleteMax1 :: Test
testDeleteMax1 = Node 4 (Node 3 Empty Empty) (Node 7 (Node 5 Empty Empty) (Node 10 Empty Empty)) ~=? t1

testDeleteMax2 :: Test
testDeleteMax2 = (Just 10,Node 4 (Node 3 Empty Empty) (Node 7 (Node 5 Empty Empty) Empty)) ~=? deleteMax t1

testDeleteMax3 :: Test
testDeleteMax3 = Node 20
                 (Node 15
                  (Node 8
                   (Node 7 Empty Empty)
                   (Node 11 Empty Empty))
                  (Node 18 Empty Empty))
                 (Node 118
                  (Node 35
                   (Node 33 Empty Empty)
                   (Node 49 Empty (Node 60 Empty Empty)))
                  (Node 166 Empty Empty))
                 ~=?
                 t2

testDeleteMax4 :: Test
testDeleteMax4 = (Just 166,
                  Node 20 (Node 15 (Node 8 (Node 7 Empty Empty) (Node 11 Empty Empty)) (Node 18 Empty Empty))
                  (Node 118 (Node 35 (Node 33 Empty Empty) (Node 49 Empty (Node 60 Empty Empty))) Empty))
                 ~=?
                 deleteMax t2

testDeleteMaxs :: Test
testDeleteMaxs = TestList["testDeleteMax1" ~: testDeleteMax1,
                          "testDeleteMax2" ~: testDeleteMax2,
                          "testDeleteMax3" ~: testDeleteMax3 ,
                          "testDeleteMax4" ~:  testDeleteMax4]

testDeleteMin1 :: Test
testDeleteMin1 = Node 4 (Node 3 Empty Empty) (Node 7 (Node 5 Empty Empty) (Node 10 Empty Empty)) ~=? t1

testDeleteMin2 :: Test
testDeleteMin2 = (Just 3,Node 4 Empty (Node 7 (Node 5 Empty Empty) (Node 10 Empty Empty))) ~=? deleteMin t1

testDeleteMin3 :: Test
testDeleteMin3 = Node 20
                 (Node 15
                  (Node 8 (Node 7 Empty Empty)
                   (Node 11 Empty Empty))
                  (Node 18 Empty Empty))
                 (Node 118 (Node 35 (Node 33 Empty Empty) (Node 49 Empty (Node 60 Empty Empty))) (Node 166 Empty Empty)) ~=? t2

testDeleteMin4 :: Test
testDeleteMin4 = (Just 7,Node 20 (Node 15 (Node 8 Empty (Node 11 Empty Empty)) (Node 18 Empty Empty)) (Node 118 (Node
 35 (Node 33 Empty Empty) (Node 49 Empty (Node 60 Empty Empty))) (Node 166 Empty Empty))) ~=? deleteMin t2

testDeleteMins :: Test
testDeleteMins = TestList["testDeleteMin1" ~: testDeleteMin1,
                          "testDeleteMin2" ~: testDeleteMin2,
                          "testDeleteMin3" ~: testDeleteMin3,
                          "testDeleteMin4" ~: testDeleteMin4]

testRemove1 :: Test
testRemove1 = Node 4 (Node 3 Empty Empty) (Node 7 (Node 5 Empty Empty) (Node 10 Empty Empty)) ~=? t1

testRemove2 :: Test
testRemove2 = Node 3 Empty (Node 7 (Node 5 Empty Empty) (Node 10 Empty Empty)) ~=? remove t1 4

testRemove3 :: Test
testRemove3 = Node 4 Empty (Node 7 (Node 5 Empty Empty) (Node 10 Empty Empty)) ~=? remove t1 3

testRemove4 :: Test
testRemove4 = Node 4 (Node 3 Empty Empty) (Node 5 Empty (Node 10 Empty Empty)) ~=? remove t1 7

testRemoves :: Test
testRemoves = TestList["testRemove1" ~: testRemove1,
                       "testRemove2" ~: testRemove2,
                       "testRemove3" ~: testRemove3,
                       "testRemove4" ~: testRemove4]

testsHUnit :: [Test]
testsHUnit = [testLeafs,
              testSizes,
              testToLists,
              testFromListToLists,
              testToSortedLists,
              testGreatValues,
              testMirrors,
              testContainss,
              testRightSons,
              testInserts,
              testValues,
              testIsBinarySearchTrees,
              testDeleteMaxs,
              testDeleteMins,
              testRemoves]

prop_insert_maintains_sbt_properties :: [Int] -> Int -> Bool
prop_insert_maintains_sbt_properties xs e = (isBSearchTree . flip insert e . fromList) xs

prop_insert_element_is_contained_in_tree :: [Int] -> Int -> Bool
prop_insert_element_is_contained_in_tree xs e = (contains . flip insert e . fromList) xs e

prop_remove_then_no_longer_contained :: [Int] -> Int -> Bool
prop_remove_then_no_longer_contained xs e =
  contains t e && not (contains (remove t e) e)
  where t = (flip insert e . fromList) xs

prop_remove_min_then_still_sbt :: [Int] -> Bool
prop_remove_min_then_still_sbt xs =
  isBSearchTree t
  where (_, t) = (deleteMin . fromList) xs

prop_remove_max_then_still_sbt :: [Int] -> Bool
prop_remove_max_then_still_sbt xs =
  isBSearchTree t
  where (_, t) = (deleteMax . fromList) xs

-- deepCheck :: Testable prop => prop -> IO ()
-- deepCheck p = quickCheckWith stdArgs { maxSuccess = 500} p
prop_always_sbt  :: [Int] -> Bool
prop_always_sbt = isBSearchTree . fromList

testsQuick = [
  testGroup "Group of tests" [
     testProperty "A (B)inary (S)earch (T)ree created fromList should always be" prop_always_sbt,
     testProperty "Element inserted is contained in the RBT" prop_insert_element_is_contained_in_tree,
     testProperty "Element inserted and removed is no longer contained" prop_remove_then_no_longer_contained,
     testProperty "insert maintains the SBT properties" prop_insert_maintains_sbt_properties,
     testProperty "DeleteMin maintains the SBT properties" prop_remove_max_then_still_sbt,
     testProperty "DeleteMax maintains the SBT properties" prop_remove_min_then_still_sbt
     ]
  ]

tests :: [Test]
tests = testsHUnit

runTests :: IO ()
runTests = runTestTT (TestList tests) >> defaultMain testsQuick >> return ()

-- tony@dagobah (0,74,) 13:24:22 ~/repo/perso/haskell-lab (master) $ make bst-tests
-- cd src/tree && runhaskell BSTTests
-- Cases: 52  Tried: 52  Errors: 0  Failures: 0
-- Group of tests:
-- A (B)inary (S)earch (T)ree created fromList should always be: [OK, passed 100 tests]
-- Element inserted is contained in the RBT: [OK, passed 100 tests]
-- Element inserted and removed is no longer contained: [OK, passed 100 tests]
-- insert maintains the SBT properties: [OK, passed 100 tests]
-- DeleteMin maintains the SBT properties: [OK, passed 100 tests]
-- DeleteMax maintains the SBT properties: [OK, passed 100 tests]

-- Properties  Total
-- Passed  6           6
-- Failed  0           0
-- Total   6           6
