module Openfile where

import System.IO (openFile, hGetContents, hClose, IOMode(ReadMode))

-- openFile :: FilePath -> IOMode -> IO Handle
-- ReadMode :: IOMode
-- hGetContents :: Handle -> IO String
-- hClose :: GHC.IO.Handle.Types.Handle -> IO ()

-- possible constructor for IOMode
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

main :: IO ()
main = do handle <- openFile "./resources/life" ReadMode
          contents <- hGetContents handle
          putStr contents
          hClose handle

-- *openfile> main
-- I know *exactly* what you mean.
-- Let me tell you why you're here.
-- You're here because you know something.
-- What you know you can't explain, but you feel it.
-- You've felt it your entire life, that there's something wrong with the world.
-- You don't know what it is, but it's there, like a splinter in your mind, driving you mad.
-- It is this feeling that has brought you to me.
-- Do you know what I'm talking about?
