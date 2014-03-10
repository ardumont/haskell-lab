module Chess where

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = filter onBoard positions
  where positions = [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
                    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
        onBoard (c', r') = c' `elem` [1..8] && r' `elem` [1..8]

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- *Chess> canReachIn3 (6,3) (8,2)
-- True
-- *Chess> canReachIn3 (6,3) (8,8)
-- True
-- *Chess> canReachIn3 (1,1) (8,8)
-- False
