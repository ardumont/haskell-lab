module OurOwnTypes where

data Point = Point Float Float deriving (Show)

data Shape = Rectangle Point Point | Circle Point Float deriving (Show)

-- Rectangle x1 y1 x2 y2
-- (x1, y1) coordinate of the upper left corner
-- (x2, y2) coordinate of the lower right corner

-- Circle cx cy r
-- (cx, cy) coordinate of the center of the circle
-- r its radius

surface :: Shape -> Float
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
surface (Circle _ r) = pi * r ^ 2

-- *OurOwnTypes> surface $ Circle (Point 10 20) 20
-- 1256.6371
-- *OurOwnTypes> surface $ Rectangle (Point 0 0) (Point 100 100)
-- 10000.0
