module SimpleIO where

import System.IO

sequentialIO :: IO ()
sequentialIO =
  mapM_ (hPutStr stdout) ["hello", " this", " will", " be", " outputed", " sequentially.\n"]

-- *SimpleIO> sequentialIO
-- hello this will be outputed sequentially.
