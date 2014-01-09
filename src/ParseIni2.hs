module ParseIni2 where

--import Char
import Control.Monad
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Data.Either
import Data.Maybe

---------------- Using Parsec - http://www.serpentine.com/blog/2007/01/31/parsing-a-simple-config-file-in-haskell/

type Config = Map.Map String String

--readConfig :: FilePath -> Config

ident :: Parser String
ident = do c <- letter <|> char '_'
           cs <- many (letter <|> digit <|> char '_' )
           return (c:cs)
        <?> "Identifier"

-- *ParseIni2> parseTest ident "ab"
-- "ab"
-- *ParseIni2> parseTest ident "abc1"
-- "abc1"
-- *ParseIni2> parseTest ident "abc1324"
-- "abc1324"

-- *ParseIni2> parseTest ident "123"
-- parse error at (line 1, column 1):
-- unexpected "1"
-- expecting Identifier

comments :: Parser ()
comments = do char '#'
              skipMany (noneOf "\r\n")
           <?> "Comment"

-- *ParseIni2> parseTest comments "# this is a comment"
-- ()
-- *ParseIni2> parseTest comments "this is not a comment"
-- parse error at (line 1, column 1):
-- unexpected "t"
-- expecting Comment

eol :: Parser ()
eol = do oneOf "\r\n"
         return ()
      <?> "end-of-line"

-- <\> char '.' <|> char '/' <|> char '{' <|> char '}' <|> char '@'

item :: Parser (String, String)
item = do key <- ident
          skipMany space
          char '='
          skipMany space
          value <- manyTill anyChar (try eol <|> try comments <|> eof)
          return (key, rstrip value)
       where rstrip :: String -> String
             rstrip = id

-- *ParseIni2> parseTest item "Extension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"
-- ("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi")
