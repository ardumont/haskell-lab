module Ch2 where
n = a `div` length xs
    where
      a  = 10
      xs = [1, 2, 3, 4, 5]

-- implementation of last
last1 :: [a] -> a
last1 xs = head (reverse xs)

last2 :: [a] -> a
last2 xs = xs !! i
           where
             i = length xs - 1

last3 :: [a] -> a
last3 xs = head (drop i xs)
           where
             i = length xs - 1
