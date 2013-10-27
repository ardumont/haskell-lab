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
                      update [] _                       = []
                      update (x@(cc, nn) : xs) e@(c, n) = case cc == c of
                        True -> let ni = nn - n in if ni <= 0 then xs else (c, ni):xs
                        _    -> x : update xs e

-- Returns a list of all anagram sentences of the given sentence.
sentenceAnagrams :: Sentence -> [Sentence]
sentenceAnagrams = undefined

type DicoOcc = [(Occurrences, [Word])]

dicoByOccurrences :: [String] -> DicoOcc
dicoByOccurrences = foldl' add []
  where add acc word = let occ = wordOccurrences word in
          case lookup occ acc of
            Nothing -> (occ, [word]) : acc
            Just ws -> (occ, word : ws) : acc -- Fixme - how to update the associative array

-- *Anagram> dicoByOccurrences ["a", "abb", "baa", "c"]
-- [([('c',1)],["c"]),([('a',2),('b',1)],["baa"]),([('a',1),('b',2)],["abb"]),([('a',1)],["a"])]

findAnagram :: Word -> [(Occurrences, a)] -> Maybe a
findAnagram w d = (flip lookup d . wordOccurrences) w

-- *Anagram> findAnagram "a" [([('a', 1), ('b', 2)], ["abb", "bab", "bba"])]
-- Nothing
-- *Anagram> findAnagram "abb" [([('a', 1), ('b', 2)], ["abb", "bab", "bba"])]
-- Just ["abb","bab","bba"]
-- *Anagram> findAnagram "bab" [([('a', 1), ('b', 2)], ["abb", "bab", "bba"])]
-- Just ["abb","bab","bba"]

-- Returns all the anagrams of a given word.
wordAnagrams :: Word -> DicoOcc -> [Word]
wordAnagrams w d = case findAnagram w d of
  Nothing -> []
  Just x  -> x

-- *Anagram> wordAnagrams "abb" [([('a', 1), ('b', 2)], ["abb"])]
-- ["abb"]
-- *Anagram> wordAnagrams "abb" [([('a', 1), ('b', 2)], ["abb", "bab", "bba"])]
-- ["abb","bab","bba"]
-- *Anagram> wordAnagrams "bba" [([('a', 1), ('b', 2)], ["abb", "bab", "bba"])]
-- ["abb","bab","bba"]
-- *Anagram> wordAnagrams "a" [([('a', 1), ('b', 2)], ["abb", "bab", "bba"])]
-- []

-- I/O

extractLines :: FilePath -> IO [String]
extractLines filePath =
  do contents <- readFile filePath
     return $ lines contents

disp :: Int -> IO [String] -> IO ()
disp n allLines =
  do ll <- allLines
     let f = take n ll in
       mapM_ putStrLn f

-- dictionary :: IO [String]
-- dictionary = extractLines "./resources/linuxwords.txt"

main :: IO ()
main = do dicoLines <- extractLines "./resources/linuxwords.txt"
          mapM_ putStrLn $ wordAnagrams "hello" (dicoByOccurrences dicoLines)
