module Monade where

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
