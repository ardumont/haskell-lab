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
