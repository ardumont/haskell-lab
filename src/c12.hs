module C12 where

fibo :: [Integer]
fibo = 0:1:[ x+y | (x,y) <- zip fibo (tail fibo)]

-- *C12> take 20 fibo
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]

fibs :: Int -> Integer
fibs n = last $ take n fibo

-- *C12> fibs 10
-- 34
-- *C12> fibs 100
-- 218922995834555169026
-- *C12> fibs 101
-- 354224848179261915075

-- *C12> head . dropWhile (<= 1000) $ fibo
-- 1597
