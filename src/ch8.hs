module Parsers where

type Parser a = String -> [(a, String)]

-- basic parsers

-- return parser: always succeeds by returning the result value
-- v without consuming the input
ret :: a -> Parser a
ret v = \ input -> [(v, input)]

-- *Parsers> ret 1 "input-string-without-consuming"
-- [(1,"input-string-without-consuming")]

-- failure parser: always fails whatever the input
failure :: Parser a
failure = \ _ -> []

-- *Parsers> failure "input"
-- []

-- parser char: fails if the input is empty, otherwise,
-- return the first item consumed from the input string

item :: Parser Char
item = \ input -> case input of
                       []     -> []
                       (x:xs) -> [(x, xs)]

-- *Parsers> item  "input-string-without-consuming"
-- [('i',"nput-string-without-consuming")]

-- abstract the application of parser using the parse function

parse :: Parser a -> Parser a
parse p input = p input

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

-- p :: Parser (Char, Char)
-- p = do
--     x <- item
--     item
--     y <- item
--     ret (x, y)

-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
-- p >>= f = \ inp -> case parse p inp of
--   [] -> []
--   [(v , out)] -> parse (f v ) out

-- p = item >>= \ x ->
--     item >>= \ _ ->
--     item >>= \ y ->
--     ret (x, y)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \ inp -> case parse p inp of
  [ ] -> parse q inp
  [(v , out)] -> [(v , out)]

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
