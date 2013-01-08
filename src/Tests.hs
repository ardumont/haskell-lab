module Tests where
import Data.Char
import Test.QuickCheck

-- *Tests> quickCheck ((\s -> s == s) :: [Char] -> Bool)
-- Loading package extensible-exceptions-0.1.1.4 ... linking ... done.
-- Loading package old-locale-1.0.0.4 ... linking ... done.
-- Loading package array-0.4.0.0 ... linking ... done.
-- Loading package deepseq-1.3.0.0 ... linking ... done.
-- Loading package time-1.4 ... linking ... done.
-- Loading package random-1.0.1.1 ... linking ... done.
-- Loading package containers-0.4.2.1 ... linking ... done.
-- Loading package pretty-1.1.1.0 ... linking ... done.
-- Loading package template-haskell ... linking ... done.
-- Loading package QuickCheck-2.4.2 ... linking ... done.
-- +++ OK, passed 100 tests.
-- *Tests> quickCheck ((\s -> (reverse.reverse) s == s) :: [Char] -> Bool)
-- +++ OK, passed 100 tests.

-- The actual worker
take5 :: [Char] -> [Char]
take5 = take 5 . filter (`elem` ['a'..'e'])

-- *Tests> take5 ("afdgh" ++ ['c'..'z'])
-- "adcde"
-- *Tests> take5 ("afh" ++ ['g'..'z'])
-- "a"
-- *Tests> take5 ("afhe" ++ ['g'..'z'])
-- "ae"

-- Invariant - length is always 5...
-- *Tests> quickCheck (\s -> length (take5 s) == 5)

-- *** Failed! Falsifiable (after 1 test):
-- ""

-- Quickcheck Says: Go screw!

-- By loosening the invariant, this pass
-- *Tests> quickCheck (\s -> length (take5 s) <= 5)
-- +++ OK, passed 100 tests.

-- *Tests> quickCheck (\s -> all (`elem` ['a'..'e']) (take5 s))
-- +++ OK, passed 100 tests.
