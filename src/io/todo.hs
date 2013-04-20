module Todo where

import System.Environment

import Data.Char (toUpper)

import Data.List (delete)

import System.IO (hPutStr,
                  hClose,
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

todoDelete :: [String] -> IO ()
todoDelete (file:index:_) = del file (read index)

todoSee :: [String] -> IO ()
todoSee (file:_) = view file

todoAdd :: [String] -> IO ()
todoAdd (file:todo) = add file (unwords todo)

dispatch :: [(String, [String] -> IO ())]
dispatch = [("del", todoDelete),
            ("see", todoSee),
            ("add", todoAdd)]

main :: IO ()
main = do (command:args) <- getArgs
          let (Just action) = (lookup command dispatch) in
            action args
