module Ch3 where

-- the efficiency of function programs

power :: (Integral a1, Num a) => a -> a1 -> a
power x k = if k == 0
            then 1
            else if (k `mod` 2) == 0
                 then power (x*x) (k `div` 2)
                 else x * power (x*x) (k `div` 2)

prodsum :: (Eq a, Num a) => a -> a
prodsum x = prod x + s x

prod :: (Eq a, Num a) => a -> a
prod 0 = 1
prod n = n * prod (n-1)

s :: (Eq a, Num a) => a -> a
s 0 = 0
s n = n + s (n-1)

--

add :: Num a => a -> a -> a
add x y = x + y

double :: Num a => a -> a
double x = add x x

-- 2 ways of solving:

-- 1. by strictness, we realise the parameters first
-- double (5 * 4) = double 20
--                = add 20 20 -- by definition of double
--                = 20 + 20   -- by definition of add
--                = 40

-- 2. call by name (outermost reducible expression)
-- double (5 * 4) = add (5 * 4) (5 * 4) -- by definition of double
--                = (5 * 4) + (5 * 4)   -- by definition of add
--                = 20 + (5 * 4)
--                = 20 + 20
--                = 40

-- Note:
-- here we must solve 2 times the expression 5 * 4
-- this can be improved by using pointers to such expressions
-- and when the expression is evaluated, we can replace the expressions by its value
-- (due to referential transparency -> a function with same parameters always return the same result)
-- we thus obtain the same numbers of steps that with the strict resolution.
-- => lazyness

-- double (5 * 4) = add (5 * 4) (5 * 4) -- by definition of double
--                = (5 * 4) + (5 * 4)   -- by definition of add
--                = 20 + 20             -- solve once and then the pointer is updated to the
--                                      -- value 20, permitting to solve immediately the
--                                      -- second expression
--                = 40

-- Consider another problem:
cancel :: a -> a -> a
cancel x _ = x

-- infinite function that never stops
fn :: Num a => a -> t
fn x = fn (x + 1)

-- Then, with strict evaluation
-- cancel 1 (fn 2) = cancel 1 (fn 3)
--                = cancel 1 (fn 4)
--                = ...

-- but with lazy
-- cancel 1 (fn 2) = 1 -- by definition of cancel

-- Order of evaluation

data List a = Nil | Cons a (List a) deriving Show

-- Given
mapcar :: (a -> b) -> (List a) -> (List b)
mapcar _ Nil = Nil
mapcar f (Cons x l) = Cons (f x) (mapcar f l)

-- *Ch3> mapcar double (Cons 1 (Cons 2 (Cons 3 Nil)))
-- Cons 2 (Cons 4 (Cons 6 Nil))

-- With the List definition, the solving of such equation renders:
-- head (mapcar double (Cons 1 (Cons 2 (Cons 3 Nil)))) = head (Cons (double 1) (Cons 2 (Cons 3 Nil)))
--                                                     = double 1
--                                                     = 1 + 1
--                                                     = 2
