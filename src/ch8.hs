module Parsers where

import Control.Monad
import Data.Char(isDigit, isUpper, isLower, isAlpha, isAlphaNum, isSpace)

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

sp :: Parser String
sp = many (sat isSpace)

-- *Parsers> parse sp " \t\n     eab"
-- [(" \t\n     ","eab")]

space :: Parser ()
space = do many (sat isSpace)
           return ()

-- *Parsers> parse space " \t\n     eab"
-- [((),"eab")]

token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x

-- *Parsers> parse (token ident) " \t\n     eab     \t\n   sdfsd"
-- [("eab","sdfsd")]

identifier :: Parser String
identifier = token ident

-- *Parsers> parse identifier " \t\n     eab     \t\n   sdfsd"
-- [("eab","sdfsd")]

natural :: Parser Int
natural = token nat

-- *Parsers> parse natural " \t\n     123     \t\n   sdfsd"
-- [(123,"sdfsd")]

symbol :: String -> Parser String
symbol s = token (string s)

-- *Parsers> parse (symbol "123") " \t\n     123     \t\n   sdfsd"
-- [("123","sdfsd")]
-- *Parsers> parse (symbol "123") " \t\n     1234     \t\n   sdfsd"
-- [("123","4     \t\n   sdfsd")]

-- parser for a non-empty list of natural numbers that ignores spacing around tokens

pa :: Parser [Int]
pa = do symbol "["
        n <- natural
        ns <- many (do symbol ","
                       natural)
        symbol "]"
        return (n:ns)

-- *Parsers> parse pa "[1,2]"
-- [([1,2],"")]
-- *Parsers> parse pa "[1]"
-- [([1],"")]
-- *Parsers> parse pa "[1,2,3]"
-- [([1,2,3],"")]
-- *Parsers> parse pa "[1,2,3] skdjfsd"
-- [([1,2,3],"skdjfsd")]
-- *Parsers> parse pa "[11,2,3] skdjfsd"
-- [([11,2,3],"skdjfsd")]
-- *Parsers> parse pa "[1,2,3,] skdjfsd"
-- []

--
-- Parser for arithmetic expressions
--

-- naive: deal with no priorities, ambiguity (can have multiple parsing trees)
-- expr ::= expr + expr | expr * expr | (expr) | nat
-- nat  ::= 0 | 1 | ... |

-- deal with priority but we still can have multiple tree for addition (e.g. a+b+c) and
-- multiplication (e.g. a*b*c)
-- expr   ::= expr + expr | term
-- term   ::= term * term | factor
-- factor ::= (expr) | nat
-- nat    ::= 0 | 1 | ... |

-- choosing association to the right for addition and multiplication
-- expr   ::= term + expr | term
-- term   ::= factor * term | factor
-- factor ::= (expr) | nat
-- nat    ::= 0 | 1 | ... |

-- which can be simplify
-- expr   ::= term (+ expr | - expr | epsilon)
-- term   ::= factor (* term | / term | epsilon)
-- factor ::= exp (^ factor | epsilon)
-- exp    ::= (expr) | nat
-- nat    ::= 0 | 1 | ... |

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
             +++ do symbol "-"
                    e <- expr
                    return (t - e)
                 +++ return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (t * f)
             +++ do symbol "/"
                    t <- term
                    return (t `div` f)
                    +++ return f

factor :: Parser Int
factor = do e <- expo
            do symbol "^"
               f <- factor
               return (e ^ f)
               +++ return e

expo :: Parser Int
expo = do symbol "("
          e <- expr
          symbol ")"
          return e
          +++ natural

-- *Parsers> parse expr "(1+2)*3"
-- [(9,"")]
-- *Parsers> parse expr "(1+2)*3+12"
-- [(21,"")]
-- *Parsers> parse expr "1+2/3"
-- [(2,"")]
-- *Parsers> parse expr "(1+2)/3"
-- [(1,"")]
-- *Parsers> parse expr "1+(2/3)"
-- [(2,"")]
-- *Parsers> parse expr "1+(2*3)"
-- [(7,"")]
-- *Parsers> parse expr "1+2*3"
-- [(7,"")]
-- *Parsers> parse expr "1+2*3-3"
-- [(4,"")]
-- *Parsers> parse expr "1+2/3-3"
-- [(-1,"")]
-- *Parsers> parse expr "2^3*4"
-- [(32,"")]
-- *Parsers> parse expr "(2^3)*4"
-- [(32,"")]
-- *Parsers> parse expr "2^(3*4)"
-- [(4096,"")]

eval :: String -> Int
eval s = case parse expr s of
           [(n,[])] -> n
           []       -> error "Invalid input"
           [(_,_)]  -> error "Bad format - there remains input impossible to parse"

-- *Parsers> eval ""
-- *** Exception: Invalid input
-- *Parsers> eval "a"
-- *** Exception: Invalid input
-- *Parsers> eval "1+"
-- *** Exception: Bad format - there remains input impossible to parse
-- *Parsers> eval "1+2"
-- 3
-- *Parsers> eval "1+2*3"
-- 7
-- *Parsers> eval "1+(2*3)"
-- 7
-- *Parsers> eval "10+1+(2*3)"
-- 17
-- *Parsers> eval "10*1+(2*3)"
-- 16
-- *Parsers> eval "(10*1)+(2*3)"
-- 16
-- *Parsers> eval " ( 10 * 1 ) + ( 2 * 3 ) "
-- 16
-- *Parsers> eval "(10*1)+(2*3)*"
-- *** Exception: Bad format - there remains input impossible to parse

int :: Parser Int
int = do symbol "-"
         n <- natural
         return (-n)
         +++ natural

-- *Parsers> parse int "-1"
-- [(-1,"")]
-- *Parsers> parse int "-10"
-- [(-10,"")]
-- *Parsers> parse int "-101"
-- [(-101,"")]
-- *Parsers> parse int "101"
-- [(101,"")]
-- *Parsers> parse int " - 101"
-- [(-101,"")]

comment :: Parser ()
comment = do symbol "--"
             many (sat (/= '\n'))
             char '\n'
             return ()

-- *Parsers> parse comment "--thisisacommentignoredtill\nnotignored"
-- [((),"notignored")]
-- *Parsers> parse comment "--this is a comment ignored till\nnotignored"
-- [((),"notignored")]
-- *Parsers> parse comment "--this is a  comment ignored till\nnotignored"
-- [((),"notignored")]
-- *Parsers> parse comment "--this is a  comment ignored 23 till\nnotignored"
-- [((),"notignored")]

-- expr ::= expr - nat | nat
-- nat  ::= 0 | 1 | ...

xpr :: Parser Int
xpr = do e <- xpr
         symbol "-"
         n <- natural
         return (e - n)
         +++ natural
