module C12 where

fibo :: [Integer]
fibo = 0:1:[ x+y | (x,y) <- zip fibo (tail fibo)]

-- *C12> take 20 fibo
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]

fibs :: Int -> Integer
fibs n = fibo !! n

-- *C12> fibs 10
-- 34
-- *C12> fibs 100
-- 218922995834555169026
-- *C12> fibs 101
-- 354224848179261915075

-- *C12> head . dropWhile (<= 1000) $ fibo
-- 1597

-- r :: a -> [a]
-- r x = x : r x

-- t :: Int -> [a] -> [a]
-- t 0 _        = []
-- t _ []       = []
-- t n (x : xs) = x : take (n-1) xs

-- rep :: Int -> a -> [a]
-- rep n = (take n) . repeat

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

p :: Show a => Tree a -> IO ()
p = (mapM_ putStrLn) . treeIndent
  where
    treeIndent Leaf          = ["-- /-"]
    treeIndent (Node lb v rb) =
      ["--" ++ (show v)] ++
      map ("  |" ++) ls ++
      ("  `" ++ r) : map ("   " ++) rs
      where
        (r:rs) = treeIndent $ rb
        ls     = treeIndent $ lb

repeatTree :: a -> Tree a
repeatTree v =
  Node t v t
  where t = repeatTree v

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _     = Leaf
takeTree _ Leaf  = Leaf
takeTree n (Node l x r) = Node (takeTree (n-1) l) x (takeTree (n-1) r)

-- *C12> p $ takeTree 3 (repeatTree 0)
-- --0
--   |--0
--   |  |--0
--   |  |  |-- /-
--   |  |  `-- /-
--   |  `--0
--   |     |-- /-
--   |     `-- /-
--   `--0
--      |--0
--      |  |-- /-
--      |  `-- /-
--      `--0
--         |-- /-
--         `-- /-

replicateTree :: Int -> a -> Tree a
replicateTree n = (takeTree n) . repeatTree

-- *C12> p $ replicateTree 3 0
-- --0
--   |--0
--   |  |--0
--   |  |  |-- /-
--   |  |  `-- /-
--   |  `--0
--   |     |-- /-
--   |     `-- /-
--   `--0
--      |--0
--      |  |-- /-
--      |  `-- /-
--      `--0
--         |-- /-
--         `-- /-
