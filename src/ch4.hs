module Ch4 where
isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

evn :: Integral a => a -> Bool
evn x = x `mod` 2 == 0
