module PropParsers where

import Parsers
import Prop

-- To deal with priorities, change the order in this function
prop :: Parser Prop
prop = propConst +++
         propVar +++
         propNot +++
         propAnd +++
         propOr  +++
         propImply +++
         propEquiv

mapBool :: [(String, Bool)]
mapBool = [("t", True), ("f", False)]

propConst :: Parser Prop
propConst = do b <- symbol "t" +++ symbol "f"
               let bool = (lookup b mapBool) in
                 case bool of
                   Just v -> return $ Const v

propVar :: Parser Prop
propVar = do l <- token letter
             return $ Var l

propNot :: Parser Prop
propNot = do symbol "!"
             a <- prop
             return (Not a)

propAnd :: Parser Prop
propAnd = do symbol "&"
             a <- prop
             b <- prop
             return (And a b)

propOr :: Parser Prop
propOr = do symbol "|"
            a <- prop
            b <- prop
            return (Or a b)

propImply :: Parser Prop
propImply = do symbol "=>"
               a <- prop
               b <- prop
               return (Imply a b)

propEquiv :: Parser Prop
propEquiv = do symbol "<=>"
               a <- prop
               b <- prop
               return (Equiv a b)

propEval :: String -> (Maybe Prop)
propEval s = case parse prop s of
  [(p, "")] -> Just p
  _         -> Nothing

-- *PropParsers> propEval "! a & b"
-- Nothing
-- *PropParsers> propEval "! & a b"
-- Just (Not (And (Var 'a') (Var 'b')))
-- *PropParsers> propEval "! & | a b c"
-- Just (Not (And (Or (Var 'a') (Var 'b')) (Var 'c')))
-- *PropParsers> propEval "=> a b"
-- Just (Imply (Var 'a') (Var 'b'))
-- *PropParsers> propEval "<=> a b"
-- Just (Equiv (Var 'a') (Var 'b'))
-- *PropParsers> propEval "<=> a true"
-- Just (Equiv (Var 'a') (Const True))
-- *PropParsers> propEval "<=> a ! true"
-- Just (Equiv (Var 'a') (Not (Const True)))
