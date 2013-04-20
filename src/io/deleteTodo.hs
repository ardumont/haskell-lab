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
                         let todos = (lines todosstr)
                             htodos = zip ([0..] :: [Int]) todos in
                           do putStrLn "Todo list:"
                              mapM_ putStrLn todos
                              putStr ("Destroy todo list? (1," ++ ((show . length) todos) ++ ")?")
                              numTodoToDel <- getLine
                              let numToDel = read numTodoToDel
                                  newTodoItems = (unlines .
                                                  (map (\(_, t) -> t)) .
                                                  (delete (htodos !! numToDel))) htodos in
                                do hPutStr tempHandle newTodoItems
                                   hClose handle
                                   hClose tempHandle
                                   removeFile file
                                   renameFile tempPath file
