module Huffman where

data CodeTree = Leaf Char Int | Fork CodeTree CodeTree [Char] Int

type Bit = Int

weight :: CodeTree -> Int
weight = undefined

chars :: CodeTree -> [Char]
chars = undefined

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
