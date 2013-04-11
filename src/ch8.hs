module Parsers where

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

-- basic parsers

-- return
-- *Parsers> return 1 "input-string-without-consuming"
-- [(1,"input-string-without-consuming")]

-- fail

-- failure parser: always fails whatever the input
fail :: Parser a
fail = P (\_ -> [])

-- *Parsers> fail "input"
-- []

-- parser char: fails if the input is empty, otherwise,
-- return the first item consumed from the input string

item :: Parser Char
item = P (\ inp -> case inp of
                       []     -> []
                       (x:xs) -> [(x, xs)])

-- *Parsers> item  "input-string-without-consuming"
-- [('i',"nput-string-without-consuming")]

-- *Parsers> parse (ret 1) "abc"
-- [(1,"abc")]
-- *Parsers> parse item "abc"
-- [('a',"bc")]
-- *Parsers> parse item "bc"
-- [('b',"c")]
-- *Parsers> parse item ""
-- []
-- *Parsers> parse failure "abc"
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

-- *Parsers> parse (item +++ ret 'd') "abcde"
-- [('a',"bcde")]
-- *Parsers> parse (item +++ ret 'd') ""
-- [('d',"")]
-- *Parsers> parse (fail +++ ret 'd') ""
-- [('d',"")]
-- *Parsers> parse (fail +++ ret 'd') "abcd"
-- [('d',"abcd")]
-- *Parsers> parse (fail +++ fail) "abcd"
-- []
