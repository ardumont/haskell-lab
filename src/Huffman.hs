module Huffman where

import Data.List

data CodeTree = Leaf Char Int | Fork CodeTree CodeTree [Char] Int deriving (Show, Eq)

type Bit = Int

weight :: CodeTree -> Int
weight (Leaf _ n)     = n
weight (Fork _ _ _ n) = n

-- *Huffman> weight (Leaf 'c' 2)
-- 2
-- *Huffman> weight (Fork (Leaf 'c' 2) (Leaf 'd' 3) ['c', 'd'] 5)
-- 5

chars :: CodeTree -> [Char]
chars (Leaf c _)      = [c]
chars (Fork _ _ cs _) = cs

-- *Huffman> chars (Leaf 'c' 2)
-- "c"
-- *Huffman> chars (Fork (Leaf 'c' 2) (Leaf 'd' 3) ['c', 'd'] 5)
-- "cd"

makeCodeTree :: CodeTree -> CodeTree -> CodeTree
makeCodeTree l r = Fork l r (chars(l) ++ chars(r)) (weight(l) + weight(r))

-- *Huffman> makeCodeTree (Leaf 'c' 1) (Leaf 'd' 2)
-- Fork (Leaf 'c' 1) (Leaf 'd' 2) "cd" 3
-- *Huffman> makeCodeTree (Fork (Leaf 'c' 1) (Leaf 'd' 2) "cd" 3) (Leaf 'd' 2)
-- Fork (Fork (Leaf 'c' 1) (Leaf 'd' 2) "cd" 3) (Leaf 'd' 2) "cdd" 5

times :: [Char] -> [(Char, Int)]
times = map (\x -> (head x, ((1+) . length . tail) x)) . group . sort

-- *Huffman> times ['1','3','4','1','1','1','2']
-- [('1',4),('2',1),('3',1),('4',1)]
-- *Huffman> times ['a','b','b','a','a','a','d']
-- [('a',4),('b',2),('d',1)]

makeOrderedLeafList :: [(Char, Int)] -> [CodeTree]
makeOrderedLeafList = (map (\(c, i) -> Leaf c i)) . (sortBy (\(_, i1) (_, i2) -> compare i1 i2))

-- *Huffman> makeOrderedLeafList [('a', 10), ('b', 5), ('d', 3), ('e', 11)]
-- [Leaf 'd' 3,Leaf 'b' 5,Leaf 'a' 10,Leaf 'e' 11]
-- *Huffman> makeOrderedLeafList [(' ', 100), ('a', 10), ('b', 5), ('d', 3), ('e', 11)]
-- [Leaf 'd' 3,Leaf 'b' 5,Leaf 'a' 10,Leaf 'e' 11,Leaf ' ' 100]

singleton :: [CodeTree] -> Bool
singleton = undefined

combine :: [CodeTree] -> [CodeTree]
combine = undefined

insertByKeepingOrder :: CodeTree -> [CodeTree] -> [CodeTree]
insertByKeepingOrder = undefined

until :: ([CodeTree] -> Bool) -> ([CodeTree] -> [CodeTree]) -> [CodeTree] -> [CodeTree]
until = undefined

createCodeTree :: [Char] -> CodeTree
createCodeTree = undefined

decode :: CodeTree -> [Bit] -> [Char]
decode = undefined

encode :: (CodeTree -> [Char]) -> [Char] ->[Bit]
encode = undefined

type CodeTable = [(Char, [Bit])]

codeBits :: CodeTable -> Char -> [Bit]
codeBits = undefined

convert :: CodeTree -> CodeTable
convert = undefined

mergeCodeTables :: CodeTable -> CodeTable -> CodeTable
mergeCodeTables = undefined

quickEncode :: (CodeTree -> [Char]) -> [Char] -> [Bit]
quickEncode = undefined
