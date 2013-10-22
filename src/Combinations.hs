module Combinations where

import Test.HUnit

combinations :: [a] -> [[a]]
combinations = foldl comb [[]]
               where comb subs sub = subs ++ map ((:) sub) subs

testCombinations1 :: Test.HUnit.Test
testCombinations1 = [[],[1],[2],[2,1],[3],[3,1],[3,2],[3,2,1]]
                    ~=?
                    combinations [1,2,3]

testCombinations2 :: Test.HUnit.Test
testCombinations2 = [[],[2],[4],[4,2],[6],[6,2],[6,4],[6,4,2],[8],[8,2],[8,4],[8,4,2],[8,6],[8,6,2],[8,6,4],[8,6,4,2]]
                    ~=?
                    combinations [2,4,6,8]

testCombinationss :: Test.HUnit.Test
testCombinationss = TestList ["testCombinations1" ~: testCombinations1,
                              "testCombinations2" ~: testCombinations2]

-- Full tests
tests :: Test.HUnit.Test
tests = TestList [testCombinationss]

main :: IO ()
main = runTestTT tests >>= print
