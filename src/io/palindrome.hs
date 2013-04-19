module Palindrome where

main :: IO ()
main = interact $ unlines . (map palindromic) . lines

palindromic :: String -> String
palindromic x = if x == reverse x
                then "palindrome"
                else "not a palindrome"

-- cat resources/palindromes | runhaskell palindrome
-- palindrome
-- palindrome
-- not a palindrome
-- palindrome
-- not a palindrome
