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

-- *PropParsers> pconst "Const true"
-- Just (Const True)
-- *PropParsers> pconst "Const True"
-- Just (Const True)
-- *PropParsers> pconst "Const false"
-- Just (Const False)
-- *PropParsers> pconst "Const False"
-- Just (Const False)
-- *PropParsers> pconst "Const nawak False"
-- Nothing
-- *PropParsers> pconst " nawak "
-- Nothing

var :: Parser Char
var = do symbol "Var"
         letter

-- *PropParsers> parse var "Var a"
-- [('a',"")]
-- *PropParsers> parse var "Var b"
-- [('b',"")]
-- *PropParsers> parse var "Var c"
-- [('c',"")]


pvar :: String -> Maybe Prop
pvar s = case parse var s of
  [(v, _)] -> Just (Var v)
  _        -> Nothing

-- *PropParsers> pvar "Var a"
-- Just (Var 'a')
-- *PropParsers> pvar "Var alskdfj"
-- Just (Var 'a')
-- *PropParsers> pvar "Var nothing"
-- Just (Var 'n')
-- *PropParsers> pvar "nawak"
-- Nothing

-- prop :: Parser Prop
-- prop = const +++ var

-- not :: Parser Prop
-- not = do symbol "Not"
--          prop
