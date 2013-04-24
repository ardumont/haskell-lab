module Life where

import IORoutine

width :: Int
width = 20

height :: Int
height = 20

type Board = [Pos]

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showcells :: Board -> IO ()
showcells b = seqn [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b c = elem c b

-- *GameOfLife> isAlive glider (2,3)
-- True
-- *GameOfLife> isAlive glider (2,4)
-- False

isEmpty :: Board -> Pos -> Bool
isEmpty b c = not (isAlive b c)

-- *GameOfLife> isEmpty glider (2,3)
-- False
-- *GameOfLife> isEmpty glider (2,4)
-- True

nbs :: Pos -> [Pos]
nbs (x,y) = map wrap [(x-1, y-1), (x-1, y), (x-1, y+1),
                      (x, y-1), (x, y+1),
                      (x+1, y-1), (x+1, y), (x+1, y+1)]

-- *GameOfLife> nbs (1,1)
-- [(0,0),(0,1),(0,2),(1,0),(1,2),(2,0),(2,1),(2,2)]
-- *GameOfLife> nbs (0, 0)
-- [(4,4),(4,5),(4,1),(5,4),(5,1),(1,4),(1,5),(1,1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width)+1,
              ((y-1) `mod` height)+1)

-- *GameOfLife> wrap (0, 0)
-- (5,5)
-- *GameOfLife> wrap (5, 5)
-- (5,5)
-- *GameOfLife> wrap (4, 5)
-- (4,5)
-- *GameOfLife> wrap (0, 5)
-- (5,5)
-- *GameOfLife> wrap (5, 0)
-- (5,5)

livenbs :: Board -> Pos -> Int
livenbs b = length . (filter (isAlive b)) . nbs

survivors :: Board -> [Pos]
survivors b = [ c | c <- b, elem (livenbs b c) [2,3] ]

-- naive because we compute for each cell
naivebirths :: Board -> [Pos]
naivebirths b = [ (x, y) | x <- [1..width],
                           y <- [1..height],
                           isEmpty b (x, y),
                           livenbs b (x, y) == 3]

-- compute from cell's neighbours the next generation
births :: Board -> [Pos]
births b = [ c | c <- rmdups (concat (map nbs b)),
                 isEmpty b c,
                 livenbs b c == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nxgen :: Board -> Board
nxgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showcells b
            wait 50000
            life (nxgen b)

wait :: Int -> IO ()
wait n = seqn [ return () | _ <- [1..n]]
