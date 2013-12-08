module SimpleIO where

import System.IO
import Control.Concurrent

sequentialIO :: IO ()
sequentialIO =
  do mapM_ (hPutStr stdout) ["hello", " this", " will", " be", " outputed", " sequentially.\n"]
     hPutStr stdout "Good bye!\n"


-- *SimpleIO> sequentialIO
-- hello this will be outputed sequentially.

concurrentIO :: IO ()
concurrentIO =
  do forkIO sequentialIO
     hPutStr stdout "The display of this string is non-deterministic."

-- *SimpleIO> concurrentIO
-- heTlhleo d itshpilsay  woifl lth ibse s toruitnpgu tiesd n osne-qdueetnetrimailnliys.t
