module DeleteTodo where

import System.IO (openFile,
                  hGetContents,
                  hPutStr,
                  hClose,
                  IOMode(ReadMode),
                  openTempFile)

import System.Directory (removeFile,
                         renameFile)

import Data.List (delete)

main :: IO ()
main = let file = "./resources/todo.org" in
  do deleteTodoTask file
     contents <- readFile file
     putStr contents


-- hGetContents :: GHC.IO.Handle.Types.Handle -> IO String

-- zip :: [a] -> [b] -> [(a, b)]

-- Prelude> zip [0..] ["a", "b"]
-- [(0,"a"),(1,"b")]

-- delete :: Eq a => a -> [a] -> [a]

-- openTempFile :: FilePath -> String -> IO (FilePath, GHC.IO.Handle.Types.Handle)

-- hPutStr :: GHC.IO.Handle.Types.Handle -> String -> IO ()

-- hClose :: GHC.IO.Handle.Types.Handle -> IO ()

-- System.Directory.removeFile :: FilePath -> IO ()

-- System.Directory.renameFile :: FilePath -> FilePath -> IO ()

deleteTodoTask :: FilePath -> IO ()
deleteTodoTask file = do handle <- openFile file ReadMode
                         (tempPath, tempHandle) <- openTempFile "." "temp"
                         todosstr <- hGetContents handle
                         let htodos = todos todosstr in
                           do putStrLn "Todo list:"
                              mapM_ putStrLn (map (\(n,t) -> (show n) ++ t) htodos)
                              putStr ("Destroy todo list? (0," ++ (range htodos) ++ ")? ")
                              numTodoToDel <- getLine
                              let numToDel = read numTodoToDel
                                  newTodoItems = newTodos htodos numToDel in
                                do hPutStr tempHandle newTodoItems
                                   hClose handle
                                   hClose tempHandle
                                   removeFile file
                                   renameFile tempPath file

todos :: String -> [(Int, String)]
todos = zip ([0..] :: [Int]) . lines

range :: [(Int, String)] -> String
range = (show . (\ x -> x-1) . length)

newTodos :: [(Int, String)] -> Int -> String
newTodos h n = (unlines . (map (\(_, t) -> t)) . (delete (h !! n))) h
