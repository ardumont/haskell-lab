module Parsers where

type Parser a = String -> [(a, String)]

-- basic parsers

-- return parser

r :: a -> Parser a
r v = \ inp -> [(v, inp)]

-- *Parsers> (r "value") "input"
-- [("value","input")]
