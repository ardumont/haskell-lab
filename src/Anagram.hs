module Anagram where

import Data.List

type Word = String

type Sentence = [Word]

type Occurrences = [(Char, Int)]

wordOccurrences :: Word -> Occurrences
wordOccurrences = map (\x -> (head x, length x)) . group . sort

join :: String -> [String] -> [Char]
join d = foldl1' (\s ns -> s ++ d ++ ns)

sentenceOccurrences :: Sentence -> Occurrences
sentenceOccurrences = wordOccurrences . (join "")

combinations :: Occurrences -> [Occurrences]
combinations =
  foldl comb [[]]
  where comb :: [Occurrences] -> (Char, Int) -> [Occurrences]
        comb ss ci = ss ++ [c:sub | sub <- ss, c <- subOccurrences ci]
        subOccurrences :: (Char, Int) -> Occurrences
        subOccurrences (c, n) = [(c, i) | i <- [1..n]]
