module OurOwnTypes where

data Shape = Rectangle Float Float Float Float | Circle Float Float Float deriving Show

-- Rectangle x1 y1 x2 y2
-- (x1, y1) coordinate of the upper left corner
-- (x2, y2) coordinate of the lower right corner

-- Circle cx cy r
-- (cx, cy) coordinate of the center of the circle
-- r its radius

surface :: Shape -> Float
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
surface (Circle _ _ r) = pi * r ^ 2

-- *OurOwnTypes> surface $ Circle 10 20 20
-- 1256.6371
-- *OurOwnTypes> surface $ Rectangle 0 0 100 100
-- 10000.0
