module PropParsers where

import Parsers
import Prop

import String (capitalize)

propConst :: Parser Prop
propConst = do symbol "const"
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

propVar :: Parser Prop
propVar = do symbol "var"
             l <- letter
             return $ Var l

-- *PropParsers> parse var "Var a"
-- [(Var 'a',"")]
-- *PropParsers> parse var "nawak"
-- []
-- *PropParsers> parse var "Var abc"
-- [(Var 'a',"bc")]

prop :: Parser Prop
prop = propConst +++
         propVar +++
         propNot +++
         propAnd +++
         propOr  +++
         propImply +++
         propEquiv

-- *PropParsers> parse prop "not var a"
-- [(Not (Var 'a'),"")]
-- *PropParsers> parse prop "const true"
-- [(Const True,"")]
-- *PropParsers> parse prop "var a"
-- [(Var 'a',"")]

propNot :: Parser Prop
propNot = do symbol "not"
             a <- prop
             return (Not a)

-- *PropParsers> parse propNot "var a"
-- []
-- *PropParsers> parse propNot "not var a"
-- [(Not (Var 'a'),"")]

propAnd :: Parser Prop
propAnd = do symbol "and"
             a <- prop
             b <- prop
             return (And a b)

-- *PropParsers> parse propAnd "and var a var b"
-- [(And (Var 'a') (Var 'b'),"")]
-- *PropParsers> parse propAnd "and var a var b"
-- [(And (Var 'a') (Var 'b'),"")]
-- *PropParsers> parse propAnd "null"
-- []

propOr :: Parser Prop
propOr = do symbol "or"
            a <- prop
            b <- prop
            return (Or a b)

propImply :: Parser Prop
propImply = do symbol "imply"
               a <- prop
               b <- prop
               return (Imply a b)

propEquiv :: Parser Prop
propEquiv = do symbol "equiv"
               a <- prop
               b <- prop
               return (Equiv a b)

propEval :: String -> (Maybe Prop)
propEval s = case parse prop s of
  [(p, "")] -> Just p
  _         -> Nothing
