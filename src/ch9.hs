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
