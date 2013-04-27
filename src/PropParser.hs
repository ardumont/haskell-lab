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

propConst :: Parser Prop
propConst = do symbol "Const"
               b <- symbol "true" +++
                    symbol "True" +++
                    symbol "False" +++
                    symbol "false"
               let bool = read (capitalize b) :: Bool in
                 return $ Const bool

-- *PropParsers> parse propConst "Const asdtrue"
-- []
-- *PropParsers> parse propConst "Const true"
-- [(Const True,"")]
-- *PropParsers> parse propConst "Const false"
-- [(Const False,"")]
-- *PropParsers> parse propConst "Const False"
-- [(Const False,"")]
-- *PropParsers> parse propConst "Const True"
-- [(Const True,"")]

var :: Parser Prop
var = do symbol "Var"
         l <- letter
         return $ Var l

-- *PropParsers> parse var "Var a"
-- [(Var 'a',"")]
-- *PropParsers> parse var "nawak"
-- []
-- *PropParsers> parse var "Var abc"
-- [(Var 'a',"bc")]
