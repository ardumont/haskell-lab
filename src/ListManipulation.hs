module ListManipulation where

-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldl :: ([[a]] -> b -> [[a]]) -> [[a]] -> [b] -> [[a]]

pack :: Eq a => [a] -> [[a]]
pack list = foldl (\ l e -> case l of
                      [[]]      -> [[e]]
                      (x:xs):ys -> if x == e then (e:x:xs):ys else [e]:(x:xs):ys)
            [[]] $ reverse list

-- *ListManipulation> pack [1,1,1,2,2,3]
-- [[1,1,1],[2,2],[3]]
