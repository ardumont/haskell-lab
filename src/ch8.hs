module Parsers where

type Parser a = String -> [(a, String)]

-- basic parsers

-- return parser: always succeeds by returning the result value v without
-- consuming the input
ret :: a -> Parser a
ret v = \ input -> [(v, input)]

-- *Parsers> ret 1 "input-string-without-consuming"
-- [(1,"input-string-without-consuming")]

-- failure parser: always fails whatever the input
failure :: Parser a
failure = \ _ -> []

-- *Parsers> failure "input"
-- []

-- parser char

item :: Parser Char
item = \ inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x, xs)]

-- *Parsers> item "abc"
-- [('a',"bc")]
                -- *Parsers> item ""
-- []
