module Huffman where
import Data.List
import Data.Maybe

data CodeTree = Leaf !Char !Int | Fork !CodeTree !CodeTree ![Char] !Int deriving (Show, Eq)

type Bit = Int

weight :: CodeTree -> Int
weight (Leaf _ n)     = n
weight (Fork _ _ _ n) = n

chars :: CodeTree -> [Char]
chars (Leaf c _)      = [c]
chars (Fork _ _ cs _) = cs

times :: [Char] -> [(Char, Int)]
times = map (\x -> (head x, length x)) . group . sort

mkFork :: CodeTree -> CodeTree -> CodeTree
mkFork l r = Fork l r  (chars l ++ chars r) $ weight l + weight r

mkOrdered :: [(Char, Int)] -> [CodeTree]
mkOrdered = map (uncurry Leaf) . sortBy (\a b -> snd a `compare`  snd b)

singleton :: [CodeTree] -> Bool
singleton [x] = True
singleton _   = False

combine :: [CodeTree] -> [CodeTree]
combine (x:y:xs) = insertBy (\a b -> weight a `compare` weight b) (mkFork x y) xs
combine x = x

fromList :: [Char] -> CodeTree
fromList = head . until singleton combine . mkOrdered . times

--------- DECODE

decode :: CodeTree -> [Bit] -> [Char]
decode cts = decode' cts []
  where decode' (Leaf c _) acc bs = decode' cts (c:acc) bs
        decode' (Fork l r _ _) acc bs = case bs of
          0 : bsss -> decode' l acc bsss
          1 : bsss -> decode' r acc bsss
          _        -> reverse acc

--------- encode

encode :: CodeTree -> [Char] -> [Bit]
encode ct = concatMap (encode' ct [])
  where encode' (Fork l r _ _) acc c | elem c (chars l) = encode' l (0:acc) c
                                     | elem c (chars r) = encode' r (1:acc) c
                                     | otherwise =  acc
        encode' _ acc _              = reverse acc



--------- QUICK ENCODE (for performance alternatives)

type CodeTable = [(Char, [Bit])]

codeBits ::  Char -> CodeTable -> Maybe [Bit]
codeBits c = lookup c

mergeCodeTables :: CodeTable -> CodeTable -> CodeTable
mergeCodeTables = union

convert :: CodeTree -> CodeTable
convert ct = foldl1' mergeCodeTables . map createCodeTable . chars $ ct
             where  createCodeTable c = [(c, encode ct [c])]

quickEncode :: CodeTree -> [Char] -> [Bit]
quickEncode ct = concat . concat . map (maybeToList . (flip codeBits $ convert ct))
