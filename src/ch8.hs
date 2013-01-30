module Parsers where

type Parser a = String -> [(a, String)]
