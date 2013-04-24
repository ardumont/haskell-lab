module IORoutine where

import System.IO

-- getChar :: IO Char
-- putChar :: Char -> IO ()
-- return :: a -> IO a

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- System.IO.getChar
           hSetEcho stdin True
           return c

getChar :: IO Char
getChar = do x <- getCh
             putChar x
             return x

getL :: IO String
getL = do x <- IORoutine.getChar
          if x == '\n'
            then return []
            else (do xs <- getL
                     return (x:xs))

putS :: String -> IO ()
putS s = case s of
  []     -> return ()
  (x:xs) -> putChar x >> putS xs

putSLn :: String -> IO ()
putSLn s = putS s >> putChar '\n'

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
