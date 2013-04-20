module File where

import Data.Char (toUpper)

capitalize :: String -> String
capitalize [] = []
capitalize (h:t) = toUpper h : t

-- *File> capitalize "this is a todo"
-- "This is a todo"

orgify :: String -> String
orgify s = "* " ++ s ++ "\n"

-- *File> orgify "this is a todo bullet"
-- "* this is a todo bullet\n"

-- appendfile :: FilePath -> String -> IO ()

addTodo :: FilePath -> IO ()
addTodo fileP = do todo <- getContents
                   appendFile fileP $ (unlines . map (orgify . capitalize) . lines) todo


main :: IO ()
main = addTodo "./resources/todo.org"
