module Parsers where

import Control.Monad
import Data.Char(isDigit)

newtype Parser a              =  P (String -> [(a,String)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

instance Monad Parser where
  -- return parser: always succeeds by returning the result value
  -- v without consuming the input
    return v                   =  P (\inp -> [(v,inp)])

  -- bind
    p >>= f                    =  P (\inp -> case parse p inp of
                                                []        -> []
                                                [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where
  -- fail
    mzero                      =  P (\_ -> [])
  -- choice
    p `mplus` q                =  P (\inp -> case parse p inp of
                                        []        -> parse q inp
                                        [(v,out)] -> [(v,out)])

-- basic parsers

-- return
-- *Parsers> return 1 "input-string-without-consuming"
-- [(1,"input-string-without-consuming")]

-- fail

-- failure parser: always fails whatever the input
fail :: Parser a
fail = mzero

-- *Parsers> Parsers.fail "input"
-- []

-- parser char: fails if the input is empty, otherwise,
-- return the first item consumed from the input string

item :: Parser Char
item = P (\ inp -> case inp of
                       []     -> []
                       (x:xs) -> [(x, xs)])

-- *Parsers> item  "input-string-without-consuming"
-- [('i',"nput-string-without-consuming")]
-- *Parsers> parse (return 1) "abc"
-- [(1,"abc")]
-- *Parsers> parse item "abc"
-- [('a',"bc")]
-- *Parsers> parse item "bc"
-- [('b',"c")]
-- *Parsers> parse item ""
-- []
-- *Parsers> parse Parsers.fail "abc"
-- []

only1and3Char :: Parser (Char, Char)
only1and3Char = do x <- item
                   item
                   y <- item
                   return (x, y)

-- *Parsers> parse only1and3Char "agc"
-- [(('a','c'),"")]
-- *Parsers> parse only1and3Char "agcsldkfjsdk"
-- [(('a','c'),"sldkfjsdk")]
-- *Parsers> parse only1and3Char "ab"
-- []

(+++) :: Parser a -> Parser a -> Parser a
(+++) = mplus

-- *Parsers> parse (item +++ Parsers.fail) "abcd"
-- [('a',"bcd")]
-- *Parsers> parse (Parsers.fail +++ item) "abcd"
-- [('a',"bcd")]
-- *Parsers> parse (Parsers.fail +++ return 'd') "abcd"
-- [('d',"abcd")]

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item; if p x then return x else Parsers.fail

-- *Parsers> parse (sat isDigit) "abc"
-- []
-- *Parsers> parse (sat isDigit) "a1bc"
-- []
-- *Parsers> parse (sat isDigit) "11a1bc"
-- [('1',"1a1bc")]

digit :: Parser Char
digit = sat isDigit

-- *Parsers> parse digit "11a1bc"
-- [('1',"1a1bc")]
-- *Parsers> parse digit "abc"
-- []
