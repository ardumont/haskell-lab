module Anagram where

import Data.List
--import Test.HUnit

type Word = String

type Sentence = [Word]

type Occurrences = [(Char, Int)]

wordOccurrences :: Word -> Occurrences
wordOccurrences = map (\x -> (head x, length x)) . group . sort

-- *Anagram> wordOccurrences "this is the last day on earth"
-- [(' ',6),('a',3),('d',1),('e',2),('h',3),('i',2),('l',1),('n',1),('o',1),('r',1),('s',3),('t',4),('y',1)]
-- *Anagram> wordOccurrences "pump it up"
-- [(' ',2),('i',1),('m',1),('p',3),('t',1),('u',2)]

join :: String -> [String] -> [Char]
join _ []      = []
join d (hs:ss) = foldl (\s ns -> s ++ d ++ ns) hs ss

-- *Anagram> join ", " ["how do you do", "this is really exciting times", "functional programming is fun"]
-- "how do you do, this is really exciting times, functional programming is fun"

sentenceOccurrences :: Sentence -> Occurrences
sentenceOccurrences = wordOccurrences . (join "")

-- *Anagram> sentenceOccurrences ["this is the last day on earth", "pump it up"]
-- [(' ',8),('a',3),('d',1),('e',2),('h',3),('i',3),('l',1),('m',1),('n',1),('o',1),('p',3),('r',1),('s',3),('t',5),('u',2),('y',1)]
