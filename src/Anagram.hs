module Anagram where

import Data.List
import Test.HUnit

type Word = String

type Sentence = [Word]

type Occurrences = [(Char, Int)]

wordOccurrences :: Word -> Occurrences
wordOccurrences = map (\x -> (head x, length x)) . group . sort

join :: String -> [String] -> [Char]
join d = foldl1' (\s ns -> s ++ d ++ ns)

sentenceOccurrences :: Sentence -> Occurrences
sentenceOccurrences = wordOccurrences . (join "")

subOccurrences :: Occurrences -> Occurrences
subOccurrences = concatMap (\(c, n) -> [(c, i) | i <- [1..n]])

-- *Anagram> subOccurrences [(' ', 5)]
-- [(' ',1),(' ',2),(' ',3),(' ',4),(' ',5)]foldl :: (a -> b -> a) -> a -> [b] -> a
-- *Anagram> subOccurrences [(' ', 5),('a', 2)]
-- [(' ',1),(' ',2),(' ',3),(' ',4),(' ',5),('a',1),('a',2)]

combinations :: Occurrences -> [Occurrences]
combinations =
  foldl comb [[]]
  where comb :: [Occurrences] -> (Char, Int) -> [Occurrences]
        comb ss o = map (addOcc o) ss
        addOcc :: (Char, Int) -> Occurrences -> Occurrences
        addOcc = (:)

-- foldl :: ([Occurrences] -> (Char, Int) -> [Occurrences]) -> [Occurrences] -> Occurrences -> [Occurrences]

-- combinations :: Occurrences -> [Occurrences]

-- foldl :: ([[(Char, Int)]] -> (Char, Int) -> [[(Char, Int)]]) -> [[(Char, Int)]] -> [(Char, Int)] -> [[(Char, Int)]]

-- TESTS

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

-- Full tests
tests :: Test.HUnit.Test
tests = TestList [testWordOccurrencess,
                  testJoins,
                  testSentenceOccurrencess]

main :: IO ()
main = runTestTT tests >>= print
