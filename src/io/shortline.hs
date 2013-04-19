module Shortlines where

main :: IO ()
main = do x <- getContents
          putStrLn $ shortLines x

shortLines :: String -> String
shortLines = (unlines . filter ((<10) . length) . lines)

-- cat resources/quotes | runhaskell shortline
-- I'm
-- trying
-- to
-- free
-- your
-- mind,
-- Neo.
