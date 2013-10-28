module Anagram where

import Data.List
import Data.Char (toLower)

type Word = String

type Sentence = [Word]

type Occurrences = [(Char, Int)]

wordOccurrences :: Word -> Occurrences
wordOccurrences = sort . map (\x -> (toLower (head x), length x)) . group

sentenceOccurrences :: Sentence -> Occurrences
sentenceOccurrences = wordOccurrences . (intercalate "")

combinations :: Occurrences -> [Occurrences]
combinations =
  foldl' comb [[]]
  where comb :: [Occurrences] -> (Char, Int) -> [Occurrences]
        comb ss ci = ss ++ [sub ++ [c] | sub <- ss, c <- subOccurrences ci]
        subOccurrences :: (Char, Int) -> Occurrences
        subOccurrences (c, n) = [(c, i) | i <- [1..n]]

substract :: Occurrences -> Occurrences -> Occurrences
substract occ = foldl' update occ
                where update :: Occurrences -> (Char, Int) -> Occurrences
                      update [] _                       = []
                      update (x@(cc, nn) : xs) e@(c, n) = case cc == c of
                        True -> let ni = nn - n in if ni <= 0 then xs else (c, ni):xs
                        _    -> x : update xs e

type DicoOcc = [(Occurrences, [Word])]

dicoByOccurrences :: [String] -> DicoOcc
dicoByOccurrences = nub . (foldl' add []) -- Fixme - using nub to destroy duplicated entries at the end
  where add acc word = let occ = wordOccurrences word in
          case lookup occ acc of
            Nothing -> (occ, [word]) : acc
            Just ws -> (occ, nws) : acc -- Fixme - how to update the associative array
                       where nws = if elem word ws then ws else word : ws -- no duplicated entries

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

--  An anagram of a sentence is formed by taking the occurrences of all the characters of
--  all the words in the sentence, and producing all possible combinations of words with those characters,
--  such that the words have to be from the dictionary.

-- Returns a list of all anagram sentences of the given sentence.
sentenceAnagrams :: Sentence -> DicoOcc -> [Sentence]
sentenceAnagrams s d =
  (filter (\x -> sum (map length x) == sum (map length s)) . nub . sentenceCompute . combinations . sentenceOccurrences) s
  where sentenceCompute :: [Occurrences] -> [Sentence]
        sentenceCompute []     = [[]]
        sentenceCompute (o:os) = case lookup o d of
          Nothing        -> sentenceCompute os
          Just anagrams  -> [y:ys | y <- anagrams, ys <- sentenceCompute oss] ++ sentenceCompute os
            where oss = map (flip substract o) os

dicoYesMan :: DicoOcc
dicoYesMan = dicoByOccurrences ["en", "as", "my",
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

dicoLinuxRulez :: DicoOcc
dicoLinuxRulez = dicoByOccurrences ["Rex", "Lin", "Zulu",
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

-- *Anagram> sentenceAnagrams ["yes", "man"] dicoYesMan
-- [["en","as","my"],["en","my","as"],["man","yes"],["men","say"],["as","en","my"],["as","my","en"],["Sean","my"],["sane","my"],["my","en","as"],["my","as","en"],["my","Sean"],["my","sane"],["say","men"],["yes","man"]]

-- *Anagram> sentenceAnagrams ["Linux", "rulez"] dicoLinuxRulez
-- [["nil","Rex","Zulu"],["nil","Zulu","Rex"],["Lin","Rex","Zulu"],["Lin","Zulu","Rex"],["Rex","nil","Zulu"],["Rex","Lin","Zulu"],["Rex","Zulu","nil"],["Rex","Zulu","Lin"],["Linux","rulez"],["rulez","Linux"],["Zulu","nil","Rex"],["Zulu","Lin","Rex"],["Zulu","Rex","nil"],["Zulu","Rex","Lin"]]

dictionaryFromFile :: FilePath -> IO DicoOcc
dictionaryFromFile filepath =
  do dicoLines <- extractLines filepath
     return $ dicoByOccurrences dicoLines

mainWordAnagrams :: String -> FilePath -> IO ()
mainWordAnagrams word filePath =
  do dicoLines <- extractLines filePath
     mapM_ putStrLn $ wordAnagrams word (dicoByOccurrences dicoLines)

printSentence :: Sentence -> IO ()
printSentence sentence = putStr "[" >> mapM_ (putStr . (++) " ") sentence >> putStrLn "]"

mainSentenceAnagrams :: [String] -> FilePath -> IO ()
mainSentenceAnagrams sentence filePath =
  do dico <- dictionaryFromFile filePath
     mapM_ printSentence $ sentenceAnagrams sentence dico
