module Prop where

import Ch7_1 (Bit, int2bin)
import Life (rmdups)

type Assoc k v = [(k, v)]

find :: Eq a => a -> Assoc a b -> b
find k t = head [ v | (k', v) <- t, k == k' ]

-- *Ch10> find 1 [(1, 2), (3, 4)]
-- 2
-- *Ch10> find 3 [(1, 2), (3, 4)]
-- 4
-- *Ch10> find 5 [(1, 2), (3, 4)]
-- *** Exception: Prelude.head: empty list

-- Tautology: logical propositions that are always true

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          deriving Show

p0 :: Prop
p0 = And (Var 'A') (Var 'B')

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply
      (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'A') (And (Var 'B') (Var 'C'))

p6 :: Prop
p6 = And (Var 'a') (Var 'b')

p7 :: Prop
p7 = Not p6

p8 :: Prop
p8 = Imply p6 p7

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var v)     = find v s
eval s (Not b)     = not $ eval s b
eval s (And a b)   = eval s a && eval s b
eval s (Or a b)    = eval s a || eval s b
eval s (Imply a b) = eval s a <= eval s b

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var v)     = [v]
vars (Not b)     = vars b
vars (And a b)   = vars a ++ vars b
vars (Or a b)    = vars a ++ vars b
vars (Imply a b) = vars a ++ vars b

-- *Ch10> vars p1
-- "AA"
-- *Ch10> vars p2
-- "ABA"
-- *Ch10> vars p3
-- "AAB"
-- *Ch10> vars p4
-- "AABB"
-- *Prop> vars p5
-- "ABC"

bbools :: Int -> [[Bool]]
bbools n = map (map convBool . make n . int2bin) [1..l]
           where
             l = (2 ^ n)
             make :: Int -> [Bit] -> [Bit]
             make m b = take m $ (b ++ repeat 0)
             convBool :: Int -> Bool
             convBool 0 = False
             convBool 1 = True

-- *Ch10> bbools 0
-- [[]]
-- *Ch10> bbools 1
-- [[True],[True]]
-- *Ch10> bbools 2
-- [[True,False],[True,False],[True,True],[True,False]]
-- *Ch10> bbools 3
-- [[True,False,False],[True,False,False],[True,True,False],[True,False,False],[True,False,True],[True,True,False],[True,True,True],[True,False,False]]

-- generate all substitutions boolean possible

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools 1 = [[True], [False]]
bools n = map (False:) bn ++ map (True:) bn
          where bn = bools (n-1)

-- *Ch10> bools 0
-- []
-- *Ch10> bools 1
-- [[True],[False]]
-- *Ch10> bools 2
-- [[False,True],[False,False],[True,True],[True,False]]
-- *Ch10> bools 3
-- [[False,False,True],[False,False,False],[False,True,True],[False,True,False],[True,False,True],[True,False,False],[True,True,True],[True,True,False]]

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = rmdups (vars p)

-- *Prop> substs p1
-- [[('A',True)],[('A',False)]]
-- *Prop> substs p2
-- [[('A',False),('B',True)],[('A',False),('B',False)],[('A',True),('B',True)],[('A',True),('B',False)]]
-- *Prop> substs p3
-- [[('A',False),('B',True)],[('A',False),('B',False)],[('A',True),('B',True)],[('A',True),('B',False)]]
-- *Prop> substs p4
-- [[('A',False),('B',True)],[('A',False),('B',False)],[('A',True),('B',True)],[('A',True),('B',False)]]

isTaut :: Prop -> Bool
isTaut p = and [ eval s p | s <- substs p ]

-- *Prop> map isTaut [p0,p1,p2,p3,p4,p5]
-- [False,False,True,False,True,False]
