module AnagramTests where
import Anagram
import Test.HUnit

testWordOccurrences1 :: Test.HUnit.Test
testWordOccurrences1 = [(' ',6),('a',3),('d',1),('e',2),('h',3),('i',2),('l',1),('n',1),('o',1),('r',1),('s',3),('t',4),('y',1)]
                       ~=?
                       wordOccurrences "this is the last day on earth"

testWordOccurrences2 :: Test.HUnit.Test
testWordOccurrences2 = [(' ',2),('i',1),('m',1),('p',3),('t',1),('u',2)] ~=? wordOccurrences "pump it up"

testWordOccurrencess :: Test.HUnit.Test
testWordOccurrencess = TestList ["testWordOccurrences1" ~: testWordOccurrences1,
                                 "testWordOccurrences2" ~: testWordOccurrences2]

testJoin1 :: Test.HUnit.Test
testJoin1 = "how do you do, this is really exciting times, functional programming is fun"
            ~=?
            join ", " ["how do you do", "this is really exciting times", "functional programming is fun"]

testJoins :: Test.HUnit.Test
testJoins = TestList ["testJoin1" ~: testJoin1]

testSentenceOccurrences1 :: Test.HUnit.Test
testSentenceOccurrences1 = [(' ',8),('a',3),('d',1),('e',2),('h',3),('i',3),('l',1),('m',1),('n',1),('o',1),('p',3),('r',1),('s',3),('t',5),('u',2),('y',1)]
                           ~=?
                           sentenceOccurrences ["this is the last day on earth", "pump it up"]

testSentenceOccurrencess :: Test.HUnit.Test
testSentenceOccurrencess = TestList [ "testSentenceOccurrences1" ~: testSentenceOccurrences1]

testCombinations1 :: Test.HUnit.Test
testCombinations1 = [[],[('a',1)],[('a',2)],[('b',1)],[('b',1),('a',1)],[('b',1),('a',2)]]
                    ~=?
                    combinations [('a', 2), ('b', 1)]

testCombinations2 :: Test.HUnit.Test
testCombinations2 = [[],[('a',1)],[('a',2)],[('b',1)],[('b',2)],[('b',1),('a',1)],[('b',2),('a',1)],[('b',1),('a',2)],[('b',2),('a',2)]]
                    ~=?
                    combinations [('a', 2), ('b', 2)]

testCombinationss :: Test.HUnit.Test
testCombinationss = TestList ["testCombinations1" ~: testCombinations1,
                              "testCombinations2" ~: testCombinations2]

-- Full tests
tests :: Test.HUnit.Test
tests = TestList [testWordOccurrencess,
                  testJoins,
                  testSentenceOccurrencess,
                  testCombinationss]

main :: IO ()
main = runTestTT tests >>= print
