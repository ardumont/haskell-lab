module Huffman where
import Data.List

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
mkOrdered = map (\(c, i) -> Leaf c i) . sortBy (\(_, i1) (_, i2) -> compare i1 i2)

singleton :: [CodeTree] -> Bool
singleton xs = (null $ tail xs) && (length xs == 1)

combine :: [CodeTree] -> [CodeTree]
combine (x:y:xs) = insertBy (\a b -> weight a `compare` weight b) (mkFork x y) xs
combine x = x

fromList :: [Char] -> CodeTree
fromList = head . until singleton combine . mkOrdered . times

--------- DECODE

decode :: CodeTree -> [Bit] -> [Char]
decode cts bs =
  internalDecode cts bs
  where internalDecode :: CodeTree -> [Bit] -> [Char]
        internalDecode (Leaf c _)     bss = c : internalDecode cts bss
        internalDecode (Fork l r _ _) bss = case bss of
          0 : bsss -> internalDecode l bsss
          1 : bsss -> internalDecode r bsss
          _        -> []

--------- ENCODE

encode :: CodeTree -> [Char] -> [Bit]
encode ct cs = concatMap (\c -> internalEncode ct c) cs
                  where internalEncode :: CodeTree -> Char -> [Bit]
                        internalEncode (Fork l r _ _) c = if elem c (chars l)
                                                          then 0 : internalEncode l c
                                                          else 1 : internalEncode r c
                        internalEncode _ _              = []

--------- QUICK ENCODE (for performance alternatives)

type CodeTable = [(Char, [Bit])]

codeBits :: CodeTable -> Char -> Maybe [Bit]
codeBits ct c = lookup c ct

mergeCodeTables :: CodeTable -> CodeTable -> CodeTable
mergeCodeTables =
  foldl maybeAdd
  where maybeAdd acc ne =
          case codeBits acc (fst ne) of
            Just _ -> acc
            _      -> ne : acc

convert :: CodeTree -> CodeTable
convert ct = foldl (\acc c -> mergeCodeTables acc (createCodeTable c)) [] . chars $ ct
             where encodeFn = encode ct
                   createCodeTable c = [(c, encodeFn([c]))]

quickEncode :: CodeTree -> [Char] -> [Bit]
quickEncode ct cs =
  concatMap toBits cs
  where codeTable :: CodeTable
        codeTable = convert ct
        toBits :: Char -> [Bit]
        toBits c = case codeBits codeTable c of
          Just x -> x
          _      -> []
