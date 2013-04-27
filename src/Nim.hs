module Nim where

import IORoutine

-- Nim is a game that is played on a board comprising five numbered rows of stars, which is initially set up as follows:
-- 1:∗∗∗∗∗
-- 2:∗∗∗∗
-- 3:∗∗∗
-- 4:∗∗
-- 5:∗
-- Two players take it in turn to remove one or more stars from the end of a single row.
-- The winner is the player who removes the last star or stars from the board.
-- Implement the game of nim in Haskell.

-- _Hint:_ represent the board as a list comprising the number of stars remaining on each row, with the initial board being =[5, 4, 3, 2, 1]=.

type Board = [Int]

makeBoard :: Int -> Board
makeBoard n = [n, n-1..1]

-- *Nim> makeBoard 5
-- [5,4,3,2,1]

computeStars :: Board -> [(Int, String)]
computeStars b = zip [0..(length b -1)] (map (flip replicate '*') b)

-- *Ch9> computeStars (makeBoard 5)
-- [(0,"*****"),(1,"****"),(2,"***"),(3,"**"),(4,"*")]

showBoard :: [(Int, String)] -> IO ()
showBoard b = seqn [writeat (x, 0) (show x ++ ": " ++ s) | (x, s) <- b]

-- *Ch9> showBoard $ computeStars (makeBoard 5)
-- 0: *****
-- 1: ****
-- 2: ***
-- 3: **
-- 4: *

wrap :: Int -> Int
wrap n = if n >= 0 then n else 0

-- *Ch9> wrap 0
-- 0
-- *Ch9> wrap (-1)
-- 0

remove :: Int -> Int -> Board -> Board
remove r n b = let (h, (v:t)) = splitAt r b
                   ns = wrap (v - n) in
               h ++ (ns:t)

-- *Ch9> remove 0 2 board
-- [3,4,3,2,1]
-- *Ch9> remove 0 10 board
-- [0,4,3,2,1]
-- *Ch9> remove 0 5 board
-- [0,4,3,2,1]

wrapStars :: Int -> Int -> Int
wrapStars r l | r < 0 = 0
              | l <= r = l
              | otherwise = r

-- *Ch9> wrapStars 10 12
-- 10
-- *Ch9> wrapStars 13 12
-- 12
-- *Ch9> wrapStars (-1) 12
-- 0

win :: Board -> Bool
win = (== 0) . sum

-- *Ch9> win [4,4,3,2,1]
-- False
-- *Ch9> win [0,0,0,0,0]
-- True

turn :: Int -> Board -> IO Board
turn p b = do showBoard $ computeStars b
              putStrLn $ "Player " ++ (show p) ++ ", on which row do you want to remove stars?"
              x <- getLine
              let r = read x in
                do putStrLn "How many stars?"
                   s <- getLine
                   let n = wrapStars (read s) (length b) in
                     return $ remove r n b

-- *Ch9> turn 1 board
-- *****
-- ****
-- ***
-- **
-- *
-- Player 1, what stars do you want to remove?
-- 1
-- [5,3,3,2,1]

nextplayer :: Int -> Int
nextplayer p = ((p+1) `mod` 2)

-- *Ch9> nextplayer 1
-- 0
-- *Ch9> nextplayer 0
-- 1

game :: Int -> Board -> IO ()
game p b = do nb <- turn p b
              if win nb
                then putStrLn $ "p" ++ (show p) ++ " won!"
                else game (nextplayer p) nb

setupGame :: IO (Int, Int)
setupGame = do putStrLn "What size for the board?"
               n <- getLine
               let size = read n in
                 do putStrLn "What player first? (0 or 1)"
                    p <- getLine
                    let player = read p in
                      return (size, player)

main :: IO ()
main = do (size, player) <- setupGame
          game player (makeBoard size)
