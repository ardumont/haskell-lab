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
singleton = ((== 1) . length)

-- *Huffman> singleton [Leaf 'd' 3,Leaf 'b' 5,Leaf 'a' 10,Leaf 'e' 11,Leaf ' ' 100]
-- False
-- *Huffman> singleton [Leaf 'd' 3]
-- True

combine :: [CodeTree] -> [CodeTree]
combine [] = []
combine [c] = [c]
combine (c1:c2:cs) = insertByKeepingOrder (makeCodeTree c1 c2) cs
                     where
                       insertByKeepingOrder :: CodeTree -> [CodeTree] -> [CodeTree]
                       insertByKeepingOrder c []           = [c]
                       insertByKeepingOrder c css@(cx:cxs) = if weight(c) <= weight(cx) then c:css
                                                             else cx : insertByKeepingOrder c cxs

-- *Huffman> combine [Leaf 'a' 10,Leaf 'b' 20, Leaf 'c' 15]
-- [Leaf 'c' 15,Fork (Leaf 'a' 10) (Leaf 'b' 20) "ab" 30]
-- *Huffman> combine [Leaf 'a' 10,Leaf 'b' 20, Leaf 'c' 40]
-- [Fork (Leaf 'a' 10) (Leaf 'b' 20) "ab" 30,Leaf 'c' 40]

until :: ([CodeTree] -> Bool) -> ([CodeTree] -> [CodeTree]) -> [CodeTree] -> [CodeTree]
until stopFn combineFn cs =
  if stopFn ncs then ncs
  else Huffman.until stopFn combineFn ncs
  where ncs :: [CodeTree]
        ncs = combineFn cs

-- *Huffman> Huffman.until singleton combine [Leaf 'a' 10,Leaf 'b' 20, Leaf 'c' 20]
-- [Fork (Leaf 'c' 20) (Fork (Leaf 'a' 10) (Leaf 'b' 20) "ab" 30) "cab" 50]
-- *Huffman> Huffman.until singleton combine [Leaf 'a' 10,Leaf 'b' 20, Leaf 'c' 20, Leaf 'd' 21]
-- [Fork (Fork (Leaf 'a' 10) (Leaf 'b' 20) "ab" 30) (Fork (Leaf 'c' 20) (Leaf 'd' 21) "cd" 41) "abcd" 71]

createCodeTree :: [Char] -> CodeTree
createCodeTree = head . (Huffman.until singleton combine) . makeOrderedLeafList . times

-- *Huffman> createCodeTree "to be or not to be"
-- Fork (Fork (Fork (Fork (Leaf 'n' 1) (Leaf 'r' 1) "nr" 2) (Leaf 'b' 2) "nrb" 4) (Leaf 'o' 4) "nrbo" 8) (Fork (Fork (Leaf 'e' 2) (Leaf 't' 3) "et" 5) (Leaf ' ' 5) "et " 10) "nrboet " 18

decode :: CodeTree -> [Bit] -> [Char]
decode cts bs =
  internalDecode cts bs
  where internalDecode :: CodeTree -> [Bit] -> [Char]
        internalDecode (Leaf c _)     bss = c : internalDecode cts bss
        internalDecode (Fork l r _ _) bss = case bss of
          0 : bsss -> internalDecode l bsss
          1 : bsss -> internalDecode r bsss
          _        -> []


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
