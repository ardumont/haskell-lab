module Parsers where

import Control.Monad
import Data.Char(isDigit, isUpper, isLower, isAlpha, isAlphaNum)

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

upper :: Parser Char
upper = sat isUpper

-- *Parsers> parse upper "Abc"
-- [('A',"bc")]
-- *Parsers> parse upper "abc"
-- []

lower :: Parser Char
lower = sat isLower

-- *Parsers> parse lower "Abc"
-- []
-- *Parsers> parse lower "abc"
-- [('a',"bc")]

letter :: Parser Char
letter = sat isAlpha

-- *Parsers> parse letter "123abc"
-- []
-- *Parsers> parse letter "a1bc"
-- [('a',"1bc")]

alphanum :: Parser Char
alphanum = sat isAlphaNum

-- *Parsers> parse alphanum "a01bc213"
-- [('a',"01bc213")]
-- *Parsers> parse alphanum "01bc213"
-- [('0',"1bc213")]
-- *Parsers> parse alphanum "=,"
-- []

char :: Char -> Parser Char
char x = sat (== x)

-- *Parsers> parse (char 'a') "bca"
-- []
-- *Parsers> parse (char 'a') "abca"
-- [('a',"bca")]

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- *Parsers> parse (string "abc") "abcdef"
-- [("abc","def")]
-- *Parsers> parse (string "abc") "def"
-- []

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v  <- p
             vs <- many p
             return (v:vs)

-- *Parsers> parse (many digit) "123abc"
-- [("123","abc")]
-- *Parsers> parse (many digit) "abc"
-- [("","abc")]
-- *Parsers> parse (many1 digit) "abc"
-- []
-- *Parsers> parse (many1 digit) "123abc"
-- [("123","abc")]
-- *Parsers> parse (many1 digit) "123abc1"
-- [("123","abc1")]
-- *Parsers> parse (many1 digit) "abc1"
-- []

-- lower-case letter followed by zero or more alphanumeric characters

-- identifier variable
ident :: Parser String
ident = do v <- lower
           vs <- many alphanum
           return (v:vs)

-- *Parsers> parse ident "a123454,"
-- [("a123454",",")]
-- *Parsers> parse ident "thisIsAnIdentifier"
-- [("thisIsAnIdentifier","")]
-- *Parsers> parse ident "thisIsAnIdentifier-,sajdfl"
-- [("thisIsAnIdentifier","-,sajdfl")]

-- natural numbers comprising one or more digits,

nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs)

-- *Parsers> parse nat "1234"
-- [(1234,"")]
-- *Parsers> parse nat "1234asbc"
-- [(1234,"asbc")]

-- spacing comprising zero or more space, tab, and newline characters
