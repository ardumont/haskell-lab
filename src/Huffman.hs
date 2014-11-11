module Huffman where
import           Data.Function
import           Data.List
import           Data.Maybe

data CodeTree = Leaf !Char !Int | Fork !CodeTree !CodeTree !String !Int deriving (Show, Eq)

type Bit = Int

weight :: CodeTree -> Int
weight (Leaf _ n)     = n
weight (Fork _ _ _ n) = n

chars :: CodeTree -> String
chars (Leaf c _)      = [c]
chars (Fork _ _ cs _) = cs

times :: String -> [(Char, Int)]
times = map (\x -> (head x, length x)) . group . sort

mkFork :: CodeTree -> CodeTree -> CodeTree
mkFork l r = Fork l r  (chars l ++ chars r) $ weight l + weight r

mkOrdered :: [(Char, Int)] -> [CodeTree]
mkOrdered = map (uncurry Leaf) . sortBy (compare `on`  snd)

singleton :: [CodeTree] -> Bool
singleton [x] = True
singleton _   = False

combine :: [CodeTree] -> [CodeTree]
combine (x:y:xs) = insertBy (compare `on` weight) (mkFork x y) xs
combine x = x

fromList :: String -> CodeTree
fromList = head . until singleton combine . mkOrdered . times

--------- DECODE

decode :: CodeTree -> [Bit] -> String
decode cts = decode' cts []
  where decode' (Leaf c _) acc bs     = decode' cts (c:acc) bs
        decode' (Fork l r _ _) acc bs = case bs of
                                          0 : bss -> decode' l acc bss
                                          1 : bss -> decode' r acc bss
                                          _       -> reverse acc

--------- encode

encode :: CodeTree -> String -> [Bit]
encode ct = concatMap (encode' ct [])
  where encode' (Fork l r _ _) acc c | c `elem` chars l = encode' l (0:acc) c
                                     | c `elem` chars r = encode' r (1:acc) c
                                     | otherwise =  acc
        encode' _ acc _              = reverse acc



--------- QUICK ENCODE (for performance alternatives)

type CodeTable = [(Char, [Bit])]

codeBits ::  Char -> CodeTable -> Maybe [Bit]
codeBits  = lookup

mergeCodeTables :: CodeTable -> CodeTable -> CodeTable
mergeCodeTables = union

convert :: CodeTree -> CodeTable
convert ct = foldl1' mergeCodeTables . map createCodeTable . chars $ ct
             where  createCodeTable c = [(c, encode ct [c])]

quickEncode :: CodeTree -> String -> [Bit]
quickEncode ct = concat . mapMaybe (`codeBits` convert ct)
