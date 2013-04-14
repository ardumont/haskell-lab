module Monade where

import Parsers

-- getChar :: IO Char
-- putChar :: Char -> IO ()
-- return :: a -> IO a

getL :: IO String
getL = do x <- getChar
          if x == '\n'
            then return []
            else (do xs <- getL
                     return (x:xs))

putS :: String -> IO ()
putS s = case s of
  []     -> return ()
  (x:xs) -> (do putChar x
                putS xs)

putSLn :: String -> IO ()
putSLn s = do putS s
              putChar '\n'

strlen :: IO ()
strlen = do putSLn "Give me some input, please:"
            inp <- getL
            putSLn ("Length: " ++ (show (length inp)) ++ " characters.")

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putS ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p s = do goto p
                 putSLn s

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

putS2 :: String -> IO ()
putS2 s = seqn [ putChar x | x <- s ]

-- ############### Calculator

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: [Char]
buttons = standard ++ extra
          where standard = "qcd=123+456-789*0()/"
                extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [ writeat (1, y) xs | (y, xs) <- zip [1..(length box)] box ]

-- The last part of the user interface is to define a function that shows a string in
-- the display of the calculator, by first clearing the display and then showing the
-- last thirteen characters of the string. In this manner, if the user deletes characters
-- from the string, they will automatically be removed from the display, and if the
-- user types more than thirteen characters, the display will appear to scroll to the
-- left as additional characters are typed.
display :: String -> IO ()
display s = do writeat (3, 2) "               "
               writeat (3, 2) (reverse (take 13 (reverse s)))

-- The calculator itself is controlled by a function calc that displays the current
-- string, and then reads a character from the keyboard without echoing it. If this
-- character is a valid button, then it is processed, otherwise we sound a beep to
-- indicate an error and continue with the same string:
calc :: String -> IO ()
calc xs = do display xs
             c <- getChar
             if elem c buttons then
               process c xs
               else do beep
                       calc xs

process :: Char -> String -> IO()
process c s
  | elem c "qQ\ESC"    = quit
  | elem c "dD\BS\DEL" = delete s
  | elem c "=\n"       = ev s
  | elem c "cC"        = clear
  | otherwise          = press c s

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete "" = calc ""
delete s = calc (init s)

ev :: String -> IO ()
ev s = case parse expr s of
           [(n,[])] -> calc (show n)
           _        -> do beep
                          calc s

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c s = calc (s ++ [c])

run :: IO ()
run = do cls
         showbox
         clear
