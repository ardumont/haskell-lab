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

-- *PropParsers> parse propConst "t"
-- [(Const True,"")]
-- *PropParsers> parse propConst "f"
-- [(Const False,"")]
-- *PropParsers> parse propConst "a"
-- []

propVar :: Parser Prop
propVar = do l <- token letter
             return $ Var l

-- *PropParsers> parse propVar "a"
-- [(Var 'a',"")]
-- *PropParsers> parse propVar "t"
-- [(Var 't',"")]
-- *PropParsers> parse propVar "na"
-- [(Var 'n',"a")]
-- *PropParsers> parse propVar "1"
-- []

propNot :: Parser Prop
propNot = do symbol "!"
             a <- prop
             return (Not a)

-- *PropParsers> parse propNot "! a"
-- [(Not (Var 'a'),"")]
-- *PropParsers> parse propNot "! asldfsd"
-- [(Not (Var 'a'),"sldfsd")]
-- *PropParsers> parse propNot "! t"
-- [(Not (Const True),"")]
-- *PropParsers> parse propNot " t"
-- []

propAnd :: Parser Prop
propAnd = do symbol "&"
             a <- prop
             b <- prop
             return (And a b)

-- *PropParsers> parse propAnd "& a b"
-- [(And (Var 'a') (Var 'b'),"")]
-- *PropParsers> parse propAnd "& a ! b"
-- [(And (Var 'a') (Not (Var 'b')),"")]
-- *PropParsers> parse propAnd "& a ! t"
-- [(And (Var 'a') (Not (Const True)),"")]
-- *PropParsers> parse propAnd "& a ! t"
-- [(And (Var 'a') (Not (Const True)),"")]
-- *PropParsers> parse propAnd "& a "
-- []

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

-- *PropParsers> parse propImply "=> t ! & a c"
-- [(Imply (Const True) (Not (And (Var 'a') (Var 'c'))),"")]
-- *PropParsers> parse propImply "=> t "
-- []

propEquiv :: Parser Prop
propEquiv = do symbol "<=>"
               a <- prop
               b <- prop
               return (Equiv a b)

-- *PropParsers> parse propEquiv "<=> t "
-- []
-- *PropParsers> parse propEquiv "<=> t ! & a c"
-- [(Equiv (Const True) (Not (And (Var 'a') (Var 'c'))),"")]

propEval :: String -> (Maybe Prop)
propEval s = case parse prop s of
  [(p, "")] -> Just p
  _         -> Nothing

-- *PropParsers> propEval "f"
-- Just (Const False)
-- *PropParsers> propEval "f"
-- Just (Const False)
-- *PropParsers> propEval "! f"
-- Just (Not (Const False))
-- *PropParsers> propEval "! a"
-- Just (Not (Var 'a'))
-- *PropParsers> propEval "& ! a b"
-- Just (And (Not (Var 'a')) (Var 'b'))
-- *PropParsers> propEval "& ! a t"
-- Just (And (Not (Var 'a')) (Const True))
-- *PropParsers> propEval "& ! a <=> t f"
-- Just (And (Not (Var 'a')) (Equiv (Const True) (Const False)))

prompt :: [String]
prompt = ["keywords: &, !, t, f, =>, <=>, and any other character",
          "Enter a Proposition:"]

askUser :: IO ()
askUser =
    do mapM_ putStrLn prompt
       p <- getLine
       let r = propEval p in
         do putStrLn (case r of
                         Just v -> (if isTaut v
                                    then "tautology!"
                                    else "Not a tautology!")
                         _    ->  "Invalid input!")
            askUser

main :: IO ()
main = askUser

-- *PropParsers> main
-- keywords: &, !, t, f, =>, <=>, and any other character
-- Enter a Proposition:
-- a
-- Not a tautology!
-- keywords: &, !, t, f, =>, <=>, and any other character
-- Enter a Proposition:
-- b
-- Not a tautology!
-- keywords: &, !, t, f, =>, <=>, and any other character
-- Enter a Proposition:
-- t
-- tautology!
-- keywords: &, !, t, f, =>, <=>, and any other character
-- Enter a Proposition:
-- f
-- Not a tautology!
-- keywords: &, !, t, f, =>, <=>, and any other character
-- Enter a Proposition:
-- ! f
-- tautology!
-- keywords: &, !, t, f, =>, <=>, and any other character
-- Enter a Proposition:
-- & t t
-- tautology!
-- keywords: &, !, t, f, =>, <=>, and any other character
-- Enter a Proposition:
-- & t ! f
-- tautology!
-- keywords: &, !, t, f, =>, <=>, and any other character
-- Enter a Proposition:
-- | a ! a
-- tautology!
-- keywords: &, !, t, f, =>, <=>, and any other character
-- Enter a Proposition:
-- | a ! b
-- Not a tautology!
-- keywords: &, !, t, f, =>, <=>, and any other character
-- Enter a Proposition:
