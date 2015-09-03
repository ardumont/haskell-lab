{-# LANGUAGE TemplateHaskell #-}

-- | Tinkering in a scratch buffer

module Scratch where

import           Control.Lens
import           Control.Monad.Reader

doSthg :: Monad m => (a -> r) -> m a -> m r
doSthg f mv = mv >>= \x -> return $ f x

-- doSthg (1+) (Just 10)
-- > Just 11
-- doSthg (1+) Nothing
-- > Nothing

doStng' :: Monad m => (a -> r) -> m a -> m r
doStng' f mv = do
  v <- mv
  return $ f v

-- doSthg' (1+) (Just 10)
-- > Just 11
-- doSthg' (1+) Nothing
-- > Nothing

-- doSthg's types looks like fmap (<$>)
-- λ> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- λ> :t (<$>)
-- (<$>) :: Functor f => (a -> b) -> f a -> f b

-- (1+) <$> (Just 10) :: Maybe Int
-- > Just 11
-- > it :: Maybe Int

-- As Maybe monad is a functor, it works.
-- instance Functor Maybe where
--   f (<$>) (Just x) = Just (f x)
--   f (<$>) Nothing = Nothing

-- Functor apply function to a wrapped value and returns the wrapped result.
-- Applicative:: functor apply wrapped function to a wrapped value and return the wrapped result.

-- >>= :: Monad m => m a -> (a -> m b) -> m b

-- data Writer w a = Writer { runWriter :: (a, w) }
-- instance Monad Writer where
--   w >>= f = do
--     let (v1, s1) = runWriter w
--     let (v2, s2) = runWriter $ f v2
--     Writer (v2, s1 ++ s2)

-- data Reader r a = Reader { runReader :: r -> a }
-- instance Monad Reader where
--   m >>= k = Reader \r -> runReader (k (runReader m r)) r

-- Reader monad

hello :: Reader String String
hello = do
    name <- ask
    return ("hello, " ++ name ++ "!\n")

bye :: Reader String String
bye = do
    name <- ask
    return ("bye, " ++ name ++ "!\n")

convo :: Reader String String
convo = do
    c1 <- hello
    c2 <- bye
    return $ c1 ++ c2

readerHelloBye :: String -> IO ()
readerHelloBye s = print $ runReader convo s

-- State monad

-- State s a = State { runState :: s -> (a, s) }

-- instance State s where
--   return a = \s -> (a, s)
--   m >>= k = State \s -> let (a, s1) = runState m s
--                         in runState (k a) s1

-- Lense

data Point = Point { _x, _y :: Double } deriving Show
data Player = Player { _loc :: Point } deriving Show
data World = World { _player :: Player } deriving Show

-- λ> player1
-- Player {_loc = Point {_x = 0.0, _y = 0.0}}
-- it :: Player
-- λ> ((location . x) `over` (+22)) player1
-- Player {_loc = Point {_x = 22.0, _y = 0.0}}
-- it :: Player

makeLenses ''Point
makeLenses ''Player
makeLenses ''World

-- λ> :t (loc `over`)
-- (loc `over`) :: (Point -> Point) -> Luigi -> Luigi
-- λ> :t ((loc.x) `over`)
-- ((loc.x) `over`) :: (Double -> Double) -> Luigi -> Luigi
-- λ> :t ((loc . x) `over` (+22))
-- ((loc . x) `over` (+22)) :: Luigi -> Luigi

initWorld :: World
initWorld = World $ Player (Point 0 0)

-- λ> :t (initWorld ^. player)
-- (initWorld ^. player) :: Player
-- λ> :t (initWorld ^. (player . loc))
-- (initWorld ^. (player . loc)) :: Point
-- λ> :t (initWorld ^. (player . loc . x))
-- (initWorld ^. (player . loc . x)) :: Double

-- change the world (just change player's coordinates)
movePlayer :: World -> World
movePlayer w = (player.loc.x) `over` (+1) $ w

-- λ> movePlayer initWorld
-- World {_player = Player {_loc = Point {_x = 1.0, _y = 0.0}}}
-- it :: World
-- λ> movePlayer (movePlayer initWorld)
-- World {_player = Player {_loc = Point {_x = 2.0, _y = 0.0}}}
-- it :: World

--main :: IO ()
-- main = readerHelloBye "tony" module Anagram where

import           Control.Arrow      ((&&&))
import           Data.Char          (toLower)
import           Data.List
import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe)
import           System.Environment

-- ######### Definition type

type Word = String

type Sentence = [Word]

type Occurrences = [(Char, Int)]

-- ######### Production code

wordOccurrences :: Word -> Occurrences
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

type DicoOcc = Map.Map Occurrences [Word]

dicoByOccurrences :: [String] -> DicoOcc
dicoByOccurrences = foldl' add Map.empty
  where add :: DicoOcc -> String -> DicoOcc
        add dico word = Map.insertWith iadd occuKey [word] dico
                       where occuKey :: Occurrences
                             occuKey = wordOccurrences word
                             iadd :: Eq a => [a] -> [a] -> [a]
                             iadd ws (w:_) = if w `elem` ws then ws else w:ws

-- Returns all the anagrams of a given word.
wordAnagrams :: Word -> DicoOcc -> [Word]
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
