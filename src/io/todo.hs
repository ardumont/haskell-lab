module Todo where

import System.Environment

import Data.Char (toUpper)

import Data.List (delete)

import System.IO (openFile,
                  hGetContents,
                  hPutStr,
                  hClose,
                  IOMode(ReadMode),
                  openTempFile)

import System.Directory (removeFile,
                         renameFile)

view :: FilePath -> IO ()
view file = do contents <- readFile file
               putStr contents

-- *Todo> view "./resources/todo.org"
-- * I know *exactly* what you mean.
-- * Let me tell you why you're here.
-- * You're here because you know something.
-- * What you know you can't explain, but you feel it.
-- * You've felt it your entire life, that there's something wrong with the world.
-- * You don't know what it is, but it's there, like a splinter in your mind, driving you mad.
-- * It is this feeling that has brought you to me.
-- * Do you know what I'm talking about?

capitalize :: String -> String
capitalize [] = []
capitalize (h:t) = toUpper h : t

-- *File> capitalize "this is a todo"
-- "This is a todo"

orgify :: String -> String
orgify s = "* " ++ s ++ "\n"

-- *File> orgify "this is a todo bullet"
-- "* this is a todo bullet\n"

add :: FilePath -> String -> IO ()
add fileP todo = appendFile fileP $
                     (unlines .
                      map (orgify . capitalize) .
                      lines) todo

addTodo :: FilePath -> IO ()
addTodo fileP = do todo <- getContents
                   add fileP todo

deleteTodoTask :: FilePath -> IO ()
deleteTodoTask file = do putStrLn "Todo list:"
                         view file
                         htodos <- loadTodos file
                         putStr ("Destroy todo list? (0," ++ (range htodos) ++ ")? ")
                         num <- getLine
                         let n = (read num) in
                           do (del file n)
                              putStrLn "Updated todo list:"
                              view file

loadTodos :: FilePath -> IO [(Int, String)]
loadTodos file = do todostr <- readFile file
                    return (todos todostr)

del :: FilePath -> Int -> IO ()
del file n = do (tempPath, tempHandle) <- openTempFile "." "temp"
                htodos <- (loadTodos file)
                let newTodoItems = (newTodos htodos n) in
                  do hPutStr tempHandle newTodoItems
                     hClose tempHandle
                     removeFile file
                     renameFile tempPath file

todos :: String -> [(Int, String)]
todos = zip ([0..] :: [Int]) . lines

range :: [(Int, String)] -> String
range = (show . (\ x -> x-1) . length)

newTodos :: [(Int, String)] -> Int -> String
newTodos h n = (unlines .
                (map (\(_, t) -> t)) .
                (delete (h !! n))) h

dispatch :: IO ()
dispatch = do args <- getArgs
              let file = (args !! 1) in
                case (args !! 0) of
                  "add" -> let todo = (args !! 2) in
                    do (add file todo)
                       view file
                  "see" -> view file
                  "del" -> let numIndexToDel = read (args !! 3) in
                    do (del file numIndexToDel)
                       view file

main :: IO ()
main = dispatch
