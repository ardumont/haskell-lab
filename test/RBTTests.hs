module RBTTests where

import           Data.List                            (nub, sort)
import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit
import           Test.QuickCheck
import           Tree.RBT

-- import Test.Framework.Options (TestOptions, TestOptions'(..))
-- import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
-- import Test.Framework.Providers.HUnit

testNoRedRed1 :: Test
testNoRedRed1 = True  ~=? noRedRed t where t = Node B (Node R (Node B Empty 5 Empty) 10 Empty) 20 Empty :: Tree Int

testNoRedRed2 :: Test
testNoRedRed2 = False ~=? noRedRed t where t = Node R (Node R (Node B Empty 5 Empty) 10 Empty) 20 Empty :: Tree Int

testNoRedRed3 :: Test
testNoRedRed3 = False ~=? noRedRed t where t = Node B (Node R (Node R Empty 5 Empty) 10 Empty) 20 Empty :: Tree Int

testNoRedRed4 :: Test
testNoRedRed4 = True  ~=? noRedRed t where t = Node B (Node R (Node B Empty 5 Empty) 10 Empty) 20 Empty :: Tree Int

testNoRedReds :: Test
testNoRedReds = TestList [testNoRedRed1, testNoRedRed2, testNoRedRed3, testNoRedRed4]

testCountRB1 :: Test
testCountRB1 = (2,4) ~=? countRB t where t = Node B (Node R (Node B Empty 0 Empty) 1 (Node B (Node R Empty 2 Empty) 3 Empty)) 4 (Node B Empty 5 Empty) :: Tree Int

testCountRB2 :: Test
testCountRB2 = (4,3) ~=? countRB t where t = Node B (Node B (Node R Empty 1 Empty) 3 (Node R Empty 2 Empty)) 4 (Node B (Node R Empty 5 Empty) 6 (Node R Empty 7 Empty)) :: Tree Int

testCountRB3 :: Test
testCountRB3 = (3,4) ~=? countRB t where t = Node B (Node B Empty 0 Empty) 1 (Node R (Node B (Node R Empty 3 Empty) 4 (Node R Empty 5 Empty)) 6 (Node B Empty 7 Empty)) :: Tree Int

testCountRBs :: Test
testCountRBs = TestList [testCountRB1, testCountRB2, testCountRB3]

testPaths1 :: Test
testPaths1 = [[(B,4),(R,1),(B,0)],
              [(B,4),(R,1),(B,3),(R,2)],
              [(B,4),(R,1),(B,3)],
              [(B,4),(B,5)]]
             ~=?
             paths (Node B
                    (Node R (makeLeaf B 0) 1 (makeLeft B 3 (makeLeaf R 2)))
                    4
                    (makeLeaf B 5))

testPaths2 :: Test
testPaths2 = [[(B,1),(B,0)],[(B,1),(R,6),(B,4),(R,3)],[(B,1),(R,6),(B,4),(R,5)],[(B,1),(R,6),(B,7)]]
             ~=?
             paths (Node B
              (makeLeaf B 0)
              1
              (Node R
                     (Node B (makeLeaf R 3) 4 (makeLeaf R 5))
                      6
                     (makeLeaf B 7)))

testPathss :: Test
testPathss = TestList ["testPaths1" ~: testPaths1, "testPaths2" ~: testPaths2]

prop_sort_list_2_RBT_to_sorted_list :: [Int] -> Bool
prop_sort_list_2_RBT_to_sorted_list xs =
  sortedResult == expectedSortedList
  where sortedResult = (toSortedList . fromList) xs
        expectedSortedList = sort sortedResult

prop_rbt_is_a_bst :: [Int] -> Bool
prop_rbt_is_a_bst xs = (isBST . fromList) xs == True

prop_rbt :: [Int] -> Bool
prop_rbt xs = (isRBT . fromList) xs == True

prop_insert_element_is_contained_in_tree :: [Int] -> Int -> Bool
prop_insert_element_is_contained_in_tree xs e =
  (contains . fromList) xs e == elem e xs

prop_fromList_2_tree :: [Int] -> Bool
prop_fromList_2_tree xs = (length . toList . fromList) xs == (length . nub) xs

prop_insert_holds_rbt_properties :: [Int] -> Int -> Bool
prop_insert_holds_rbt_properties xs e = (isRBT . flip insert e . fromList) xs == True

-- deepCheck :: Testable prop => prop -> IO ()
-- deepCheck p = quickCheckWith stdArgs { maxSuccess = 500} p

testsQuick = [
  testGroup "Group of tests" [
     testProperty "A (R)ed (B)lack (T)ree should be a (B)inary (S)earch (T)ree" prop_rbt_is_a_bst,
     testProperty "Insert holds the RBT properties" prop_insert_holds_rbt_properties,
     testProperty "fromList should always create a RBT" prop_rbt,
     testProperty "Element inserted is contained in the RBT" prop_insert_element_is_contained_in_tree,
     testProperty "Sorted list from a RBT Should return a sorted list" prop_sort_list_2_RBT_to_sorted_list,
     testProperty "Length of the input list == length of the output list minus duplicates " prop_fromList_2_tree
     ]
  ]

testsHUnit :: [Test]
testsHUnit = [testCountRBs
             ,testNoRedReds
             ,testPathss]

tests :: [Test]
tests = testsHUnit

runTests :: IO ()
runTests = runTestTT (TestList tests) >> defaultMain testsQuick

-- tony@dagobah (1,06,) 13:43:47 ~/repo/perso/haskell-lab (master) $ make rbt-tests
-- cd src/tree && runhaskell RBTTests
-- Cases: 7  Tried: 7  Errors: 0  Failures: 0
-- Group of tests:
-- A (R)ed (B)lack (T)ree should be a (B)inary (S)earch (T)ree: [OK, passed 100 tests]
-- Insert holds the RBT properties: [OK, passed 100 tests]
-- fromList should always create a RBT: [OK, passed 100 tests]
-- Element inserted is contained in the RBT: [OK, passed 100 tests]
-- Sorted list from a RBT Should return a sorted list: [OK, passed 100 tests]
-- Length of the input list == length of the output list minus duplicates : [OK, passed 100 tests]

-- Properties  Total
-- Passed  6           6
-- Failed  0           0
-- Total   6           6

-- ######### Trying out to make a tree a generator

-- instance Arbitrary (Tree a) where
--   arbitrary = oneof [ return Empty, arbitrary ]

-- sizedArbTestTree :: Ord a => Int -> Gen (Tree a)
-- sizedArbTestTree 0 = return Empty
-- sizedArbTestTree n =
--   do c <- arbitrary
--      t <- sizedArbTestTree (n-1)
--      return $ insert t c

-- instance Arbitrary (Tree a) where
--   arbitrary = sized sizedArbTestTree
