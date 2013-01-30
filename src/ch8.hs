module Parsers where

type Parser a = String -> [(a, String)]

-- basic parsers

-- return parser

r :: a -> Parser a
r v = \ inp -> [(v, inp)]

-- *Parsers> (r "value") "input"
-- [("value","input")]

-- failure parser
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
