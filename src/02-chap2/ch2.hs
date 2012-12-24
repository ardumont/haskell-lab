module Ch2 where
-- fix problems
n = a `div` length xs
    where
      a  = 10
      xs = [1, 2, 3, 4, 5]

-- implementation of last
last1 :: [a] -> a
last1 xs = head (reverse xs)

last2 :: [a] -> a
last2 xs = xs !! (length xs - 1)

last3 :: [a] -> a
last3 xs = head (drop i xs)
           where
             i = length xs - 1

-- implementation of init: removes the last element from a non-empty list
init1 :: [a] -> [a]
init1 xs = take (length xs - 1) xs

init2 :: [a] -> [a]
init2 xs  = reverse (drop 1 (reverse xs))

init3 :: [a] -> [a]
init3 xs  = reverse (tail (reverse xs))
