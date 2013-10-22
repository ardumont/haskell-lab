module Anagram where

import Data.List

type Word = String

type Sentence = [Word]

type Occurrences = [(Char, Int)]

wordOccurrences :: Word -> Occurrences
wordOccurrences = map (\x -> (head x, length x)) . group . sort

sentenceOccurrences :: Sentence -> Occurrences
sentenceOccurrences = wordOccurrences . (intercalate "")

combinations :: Occurrences -> [Occurrences]
combinations =
  foldl' comb [[]]
  where comb :: [Occurrences] -> (Char, Int) -> [Occurrences]
        comb ss ci = ss ++ [c:sub | sub <- ss, c <- subOccurrences ci]
        subOccurrences :: (Char, Int) -> Occurrences
        subOccurrences (c, n) = [(c, i) | i <- [1..n]]

substract :: Occurrences -> Occurrences -> Occurrences
substract occ = foldl' update occ
                where update :: Occurrences -> (Char, Int) -> Occurrences
                      update o e@(c, n) =
                        case lookup c o of
                          Nothing -> o
                          Just i  -> if ni <= 0
                                     then nl
                                     else (c, ni) : nl
                            where ni = i - n
                                  nl = deleteBy (\ x y -> fst x == fst y) e o

-- *Anagram> substract [('x', 3), ('a', 2), ('b', 1)] [('x', 1), ('a', 2)]
-- [('x',2),('b',1)]
-- *Anagram> substract [] [('x', 1), ('a', 2)]
-- []
