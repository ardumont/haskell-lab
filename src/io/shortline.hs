module Shortlines where

-- interact :: (String -> String) -> IO ()

main :: IO ()
main = interact (unlines . filter ((<10) . length) . lines)

-- cat resources/quotes | runhaskell shortline
-- I'm
-- trying
-- to
-- free
-- your
-- mind,
-- Neo.
