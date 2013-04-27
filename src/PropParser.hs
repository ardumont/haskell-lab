module PropParsers where

import Parsers
import Prop

import String (capitalize)

-- data Prop = Const Bool
--           | Var Char
--           | Not Prop
--           | And Prop Prop
--           | Imply Prop Prop
--           | Or Prop Prop
--           | Equiv Prop Prop
--           deriving Show

propConst :: Parser Bool
propConst = do symbol "Const"
               b <- symbol "true" +++
                    symbol "True" +++
                    symbol "False" +++
                    symbol "false"
               return $ read (capitalize b)

pconst :: String -> Maybe Prop
pconst s = case parse propConst s of
  [(b, _)] -> Just (Const b)
  _        -> Nothing

-- *PropParsers> parse const "Const true"
-- [("true","")]
-- *PropParsers> parse const "Const True"
-- [("True","")]
-- *PropParsers> parse const "Const False"
-- [("False","")]
-- *PropParsers> parse const "Const false"
-- [("false","")]
-- *PropParsers> parse const "Const nawak"
-- []
-- *PropParsers> parse const " nawak"
-- []
-- *PropParsers> parse const " True"
-- []

var :: Parser Char
var = do symbol "Var"
         letter

-- *PropParsers> parse var "Var a"
-- [('a',"")]
-- *PropParsers> parse var "Var b"
-- [('b',"")]
-- *PropParsers> parse var "Var c"
-- [('c',"")]

-- prop :: Parser Prop
-- prop = const +++ var

-- not :: Parser Prop
-- not = do symbol "Not"
--          prop
