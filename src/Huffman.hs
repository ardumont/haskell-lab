module Huffman where

data CodeTree = Leaf Char Int | Fork CodeTree CodeTree [Char] Int

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

makeCodeTree :: CodeTree -> CodeTree
makeCodeTree = undefined

times :: [Char] -> [(Char, Int)]
times = undefined

makeOrderedLeafList :: [(Char, Int)] -> [CodeTree]
makeOrderedLeafList = undefined

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
