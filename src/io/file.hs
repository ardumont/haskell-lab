module File where

import Data.Char (toUpper)

-- openFile :: FilePath -> IOMode -> IO Handle
-- ReadMode :: IOMode
-- hGetContents :: Handle -> IO String
-- hClose :: GHC.IO.Handle.Types.Handle -> IO ()

-- possible constructor for IOMode
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

-- withFile
--   :: FilePath
--      -> IOMode -> (GHC.IO.Handle.Types.Handle -> IO r) -> IO r

-- readFile :: FilePath -> IO String

main :: IO ()
main =
  do writeContentOfAFile "./resources/life" "./resources/life-to-upper"
     displayContentOfFile "./resources/life-to-upper"

-- *Openfile> main
-- I KNOW *EXACTLY* WHAT YOU MEAN.
-- LET ME TELL YOU WHY YOU'RE HERE.
-- YOU'RE HERE BECAUSE YOU KNOW SOMETHING.
-- WHAT YOU KNOW YOU CAN'T EXPLAIN, BUT YOU FEEL IT.
-- YOU'VE FELT IT YOUR ENTIRE LIFE, THAT THERE'S SOMETHING WRONG WITH THE WORLD.
-- YOU DON'T KNOW WHAT IT IS, BUT IT'S THERE, LIKE A SPLINTER IN YOUR MIND, DRIVING YOU MAD.
-- IT IS THIS FEELING THAT HAS BROUGHT YOU TO ME.
-- DO YOU KNOW WHAT I'M TALKING ABOUT?

displayContentOfFile :: FilePath -> IO ()
displayContentOfFile file = do contents <- readFile file
                               putStr contents

-- *openfile> main
-- I know *exactly* what you mean.
-- Let me tell you why you're here.
-- You're here because you know something.
-- What you know you can't explain, but you feel it.
-- You've felt it your entire life, that there's something wrong with the world.
-- You don't know what it is, but it's there, like a splinter in your mind, driving you mad.
-- It is this feeling that has brought you to me.
-- Do you know what I'm talking about?

-- writeInAFile :: IO ()

writeContentOfAFile :: FilePath -> FilePath -> IO ()
writeContentOfAFile fileIn fileOut =
  do contents <- readFile fileIn
     writeFile fileOut (map toUpper contents)

-- *Openfile> writeContentOfAFile "./resources/life" "./resources/lifeupper"
-- *Openfile> displayContentOfFile "./resources/lifeupper"
-- I KNOW *EXACTLY* WHAT YOU MEAN.
-- LET ME TELL YOU WHY YOU'RE HERE.
-- YOU'RE HERE BECAUSE YOU KNOW SOMETHING.
-- WHAT YOU KNOW YOU CAN'T EXPLAIN, BUT YOU FEEL IT.
-- YOU'VE FELT IT YOUR ENTIRE LIFE, THAT THERE'S SOMETHING WRONG WITH THE WORLD.
-- YOU DON'T KNOW WHAT IT IS, BUT IT'S THERE, LIKE A SPLINTER IN YOUR MIND, DRIVING YOU MAD.
-- IT IS THIS FEELING THAT HAS BROUGHT YOU TO ME.
-- DO YOU KNOW WHAT I'M TALKING ABOUT?
