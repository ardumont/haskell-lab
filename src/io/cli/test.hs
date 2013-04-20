module Test where

import System.Environment

-- *DeleteTodo> :browse System.Environment
-- getArgs :: IO [String]
-- getEnv :: String -> IO String
-- getEnvironment :: IO [(String, String)]
-- getProgName :: IO String
-- withArgs :: [String] -> IO a -> IO a
-- withProgName :: String -> IO a -> IO a

main :: IO ()
main = do args <- getArgs
          progName <- getProgName
          putStrLn "args:"
          mapM_ putStrLn args
          putStrLn ("progName: " ++ progName)
