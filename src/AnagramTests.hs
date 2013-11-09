module AnagramTests where

import Anagram hiding (main)
import Test.HUnit
import qualified Data.Map as Map

-- ######### Tests

testWordOccurrences1 :: Test.HUnit.Test
testWordOccurrences1 = [(' ',6),('a',3),('d',1),('e',2),('h',3),('i',2),('l',1),('n',1),('o',1),('r',1),('s',3),('t',4),('y',1)]
                       ~=?
                       wordOccurrences "This is the last day on earth"

testWordOccurrences2 :: Test.HUnit.Test
testWordOccurrences2 = [(' ',2),('i',1),('m',1),('p',3),('t',1),('u',2)] ~=? wordOccurrences "pump it up"

testWordOccurrencess :: Test.HUnit.Test
testWordOccurrencess = TestList ["testWordOccurrences1" ~: testWordOccurrences1,
                                 "testWordOccurrences2" ~: testWordOccurrences2]

testSentenceOccurrences1 :: Test.HUnit.Test
testSentenceOccurrences1 = [(' ',8),('a',3),('d',1),('e',2),('h',3),('i',3),('l',1),('m',1),('n',1),('o',1),('p',3),('r',1),('s',3),('t',5),('u',2),('y',1)]
                           ~=?
                           sentenceOccurrences ["this is the last day on earth", "pump it up"]

testSentenceOccurrencess :: Test.HUnit.Test
testSentenceOccurrencess = TestList [ "testSentenceOccurrences1" ~: testSentenceOccurrences1]

testCombinations1 :: Test.HUnit.Test
testCombinations1 = [[],[('a',1)],[('a',2)],[('b',1)],[('a',1),('b',1)],[('a',2),('b',1)]]
                    ~=?
                    combinations [('a', 2), ('b', 1)]

testCombinations2 :: Test.HUnit.Test
testCombinations2 = [[],[('a',1)],[('a',2)],[('b',1)],[('b',2)],[('a',1),('b',1)],[('a',1),('b',2)],[('a',2),('b',1)],[('a',2),('b',2)]]
                    ~=?
                    combinations [('a', 2), ('b', 2)]

testCombinationss :: Test.HUnit.Test
testCombinationss = TestList ["testCombinations1" ~: testCombinations1,
                              "testCombinations2" ~: testCombinations2]

testSubstract1 :: Test.HUnit.Test
testSubstract1 = [('x',2),('b',1)]
                 ~=?
                 substract [('x', 3), ('a', 2), ('b', 1)] [('x', 1), ('a', 2)]

testSubstract2 :: Test.HUnit.Test
testSubstract2 = []
                 ~=?
                 substract [] [('x', 1), ('a', 2)]

testSubstract3 :: Test.HUnit.Test
testSubstract3 = [('b',1)]
                 ~=?
                 substract [('x', 3), ('a', 2), ('b', 1)] [('x', 3), ('a', 2)]

testSubstracts :: Test.HUnit.Test
testSubstracts = TestList [ "testSubstract1" ~: testSubstract1,
                            "testSubstract2" ~: testSubstract2,
                            "testSubstract3" ~: testSubstract3]

testDicoByOccurrences1 :: Test.HUnit.Test
testDicoByOccurrences1 = Map.fromList [([('a',1)],["a"]),([('a',1),('b',2)],["abb"]),([('a',2),('b',1)],["baa"]),([('c',1)],["c"])]
                         ~=?
                         dicoByOccurrences ["a", "abb", "baa", "c"]

testDicoByOccurrences2 :: Test.HUnit.Test
testDicoByOccurrences2 = Map.fromList [([('a',1)],["a"]),([('a',1),('b',2)],["abb","bab"]),([('a',2),('b',1)],["baa"]),([('c',1)],["c"])]
                         ~=?
                         dicoByOccurrences ["a", "abb", "baa", "abb", "bab", "c"]

testDicoByOccurrencess :: Test.HUnit.Test
testDicoByOccurrencess = TestList ["testDicoByOccurrences1" ~: testDicoByOccurrences1,
                                   "testDicoByOccurrences2" ~: testDicoByOccurrences2]

dicoOcc :: [(Occurrences, [Word])] -> DicoOcc
dicoOcc l = Map.fromList l :: DicoOcc

testWordAnagrams1 :: Test.HUnit.Test
testWordAnagrams1 = ["abb"] ~=? (wordAnagrams "abb" $ dicoOcc [([('a', 1), ('b', 2)], ["abb"])])

testWordAnagrams2 :: Test.HUnit.Test
testWordAnagrams2 = ["abb","bab","bba"] ~=? (wordAnagrams "abb" $ dicoOcc [([('a', 1), ('b', 2)], ["abb", "bab", "bba"])])

testWordAnagrams3 :: Test.HUnit.Test
testWordAnagrams3 = ["abb","bab","bba"] ~=? (wordAnagrams "bba" $ dicoOcc [([('a', 1), ('b', 2)], ["abb", "bab", "bba"])])

testWordAnagrams4 :: Test.HUnit.Test
testWordAnagrams4 = [] ~=? (wordAnagrams "a" $ dicoOcc [([('a', 1), ('b', 2)], ["abb", "bab", "bba"])])

testWordAnagrams :: Test.HUnit.Test
testWordAnagrams = TestList ["testWordAnagrams1" ~: testWordAnagrams1,
                             "testWordAnagrams2" ~: testWordAnagrams2,
                             "testWordAnagrams3" ~: testWordAnagrams3,
                             "testWordAnagrams4" ~: testWordAnagrams4]

testSentenceAnagrams1 :: Test.HUnit.Test
testSentenceAnagrams1 =  [["en","as","my"],
                          ["en","my","as"],
                          ["man","yes"],
                          ["men","say"],
                          ["as","en","my"],
                          ["as","my","en"],
                          ["sane","my"],
                          ["Sean","my"],
                          ["my","en","as"],
                          ["my","as","en"],
                          ["my","sane"],
                          ["my","Sean"],
                          ["say","men"],
                          ["yes","man"]]
                        ~=?
                        sentenceAnagrams ["yes", "man"] dicoYesMan
  where dicoYesMan = dicoByOccurrences ["en", "as", "my",
                                        "en", "my", "as",
                                        "man", "yes", "men",
                                        "say", "as", "en",
                                        "my", "as", "my",
                                        "en", "sane", "my",
                                        "Sean", "my", "my",
                                        "en", "as", "my",
                                        "as", "en", "my",
                                        "sane", "my", "Sean",
                                        "say", "men", "yes",
                                        "man"]

testSentenceAnagrams2 :: Test.HUnit.Test
testSentenceAnagrams2 = [["Lin","Rex","Zulu"],
                         ["Lin","Zulu","Rex"],
                         ["nil","Rex","Zulu"],
                         ["nil","Zulu","Rex"],
                         ["null","Rex","Uzi"],
                         ["null","Uzi","Rex"],
                         ["Rex","Lin","Zulu"],
                         ["Rex","nil","Zulu"],
                         ["Rex","null","Uzi"],
                        ["Rex","Uzi","null"],
                         ["Rex","Zulu","Lin"],
                         ["Rex","Zulu","nil"],
                         ["Linux","rulez"],
                         ["Uzi","null","Rex"],
                         ["Uzi","Rex","null"],
                         ["Zulu","Lin","Rex"],
                         ["Zulu","nil","Rex"],
                         ["Zulu","Rex","Lin"],
                         ["Zulu","Rex","nil"],
                         ["rulez","Linux"]]
                        ~=?
                        sentenceAnagrams ["Linux", "rulez"] dicoLinuxRulez
  where dicoLinuxRulez = dicoByOccurrences ["Rex", "Lin", "Zulu",
                                            "nil", "Zulu", "Rex",
                                            "Rex", "nil", "Zulu",
                                            "Zulu", "Rex", "Lin",
                                            "null", "Uzi", "Rex",
                                            "Rex", "Zulu", "Lin",
                                            "Uzi", "null", "Rex",
                                            "Rex", "null", "Uzi",
                                            "null", "Rex", "Uzi",
                                            "Lin", "Rex", "Zulu",
                                            "nil", "Rex", "Zulu",
                                            "Rex", "Uzi", "null",
                                            "Rex", "Zulu", "nil",
                                            "Zulu", "Rex", "nil",
                                            "Zulu", "Lin", "Rex",
                                            "Lin", "Zulu", "Rex",
                                            "Uzi", "Rex", "null",
                                            "Zulu", "nil", "Rex",
                                            "rulez", "Linux",
                                            "Linux", "rulez"]

testSentenceAnagrams :: Test.HUnit.Test
testSentenceAnagrams = TestList ["testSentenceAnagrams1" ~: testSentenceAnagrams1,
                                 "testSentenceAnagrams2" ~: testSentenceAnagrams2]

-- Full tests
tests :: Test.HUnit.Test
tests = TestList [testWordOccurrencess,
                  testSentenceOccurrencess,
                  testCombinationss,
                  testSubstracts,
                  testDicoByOccurrencess,
                  testWordAnagrams,
                  testSentenceAnagrams]

main :: IO ()
main = runTestTT tests >>= print

-- *Anagram> runTestTT tests
-- Cases: 18  Tried: 18  Errors: 0  Failures: 0
-- Counts {cases = 18, tried = 18, errors = 0, failures = 0}
