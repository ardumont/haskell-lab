module Ch9 where

import IORoutine
import Data.List (repeat)

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

rows :: Int
rows = 5

type Board = [Int]

board :: Board
board = [rows,(rows-1)..1]

stars :: a -> Int -> [a]
stars e n = take n $ repeat e

-- *Ch9> stars '*' 10
-- "**********"

display :: Board -> [String]
display b = map (stars '*') b

-- *Ch9> display board
-- ["*****","****","***","**","*"]

-- write :: Pos -> String -> IO ()
-- write (x,y) s = mapM_ (do

-- showBoard :: Board -> IO ()
-- showBoard = seqn [writeat p "O" | p <- b]
