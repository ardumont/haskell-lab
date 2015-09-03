module Anagram where

import           Control.Arrow      ((&&&))
import           Data.Char          (toLower)
import           Data.List
import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe)
import           System.Environment

-- ######### Definition type

type Word' = String

type Sentence = [Word']

type Occurrences = [(Char, Int)]

-- ######### Production code

wordOccurrences :: Word' -> Occurrences
wordOccurrences = map ((&&&) head length) . group . sort . map toLower

sentenceOccurrences :: Sentence -> Occurrences
sentenceOccurrences = wordOccurrences . intercalate ""

combinations :: Occurrences -> [Occurrences]
combinations =
  foldl' comb [[]]
  where comb :: [Occurrences] -> (Char, Int) -> [Occurrences]
        comb ss ci = ss ++ [sub ++ [c] | sub <- ss, c <- subOccurrences ci]
        subOccurrences :: (Char, Int) -> Occurrences
        subOccurrences (c, n) = [(c, i) | i <- [1..n]]

substract :: Occurrences -> Occurrences -> Occurrences
substract =
  foldl' update
  where update :: Occurrences -> (Char, Int) -> Occurrences
        update [] _                       = []
        update (x@(cc, nn) : xs) e@(c, n) =
          if cc == c
          then let ni = nn - n in if ni <= 0 then xs else (c, ni):xs
          else x : update xs e

type DicoOcc = Map.Map Occurrences [Word']

dicoByOccurrences :: [String] -> DicoOcc
dicoByOccurrences = foldl' add Map.empty
  where add :: DicoOcc -> String -> DicoOcc
        add dico word = Map.insertWith iadd occuKey [word] dico
                       where occuKey :: Occurrences
                             occuKey = wordOccurrences word
                             iadd :: Eq a => [a] -> [a] -> [a]
                             iadd ws (w:_) = if w `elem` ws then ws else w:ws

-- Returns all the anagrams of a given word.
wordAnagrams :: Word' -> DicoOcc -> [Word']
wordAnagrams w d =
  fromMaybe [] $ (flip Map.lookup d . wordOccurrences) w

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
  (filteringSentencesOnOccurrence . nub . sentenceCompute . combinations) sentenceOccurrenceRef
  where filteringSentencesOnOccurrence :: [Sentence] -> [Sentence]
        filteringSentencesOnOccurrence = filter (\x -> sentenceOccurrences x == sentenceOccurrenceRef)
        sentenceOccurrenceRef :: Occurrences
        sentenceOccurrenceRef = sentenceOccurrences s
        sentenceCompute :: [Occurrences] -> [Sentence]
        sentenceCompute []     = [[]]
        sentenceCompute (o:os) = case Map.lookup o d of
          Nothing        -> sentenceCompute os
          Just anagrams  -> [y:ys | y <- anagrams, ys <- sentenceCompute oss] ++ sentenceCompute os
            where oss = map (`substract` o) os

dictionaryFromFile :: FilePath -> IO DicoOcc
dictionaryFromFile filepath =
  do dicoLines <- extractLines filepath
     return $ dicoByOccurrences dicoLines

mainWordAnagrams :: String -> FilePath -> IO ()
mainWordAnagrams word filePath =
  do dicoLines <- extractLines filePath
     mapM_ putStrLn $ wordAnagrams word (dicoByOccurrences dicoLines)

printSentence :: Sentence -> IO ()
printSentence sentence = putStr "[" >> mapM_ (putStr . (++) " ") sentence >> putStrLn " ]"

mainSentenceAnagrams :: [String] -> FilePath -> IO ()
mainSentenceAnagrams sentence filePath =
  do dico <- dictionaryFromFile filePath
     mapM_ printSentence $ sentenceAnagrams sentence dico

main :: IO ()
main = do args <- getArgs
          mainSentenceAnagrams args "../resources/linuxwords.txt"
