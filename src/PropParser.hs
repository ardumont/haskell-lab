module PropParsers where

import Parsers

-- data Prop = Const Bool
--           | Var Char
--           | Not Prop
--           | And Prop Prop
--           | Imply Prop Prop
--           | Or Prop Prop
--           | Equiv Prop Prop
--           deriving Show

const :: Parser String
const = do symbol "Const"
           symbol "true" +++
             symbol "True" +++
             symbol "False" +++
             symbol "false"

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
