module Chess where

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = filter onBoard positions
  where positions = [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
                    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
        onBoard (c', r') = c' `elem` [1..8] && r' `elem` [1..8]
