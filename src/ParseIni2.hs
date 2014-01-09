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

comment :: Parser ()
comment = do char '#'
             skipMany (noneOf "\r\n")
           <?> "Comment"

-- *ParseIni2> parseTest comment "# this is a comment"
-- ()
-- *ParseIni2> parseTest comment "this is not a comment"
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
          value <- manyTill anyChar (try eol <|> try comment <|> eof)
          return (key, rstrip value)
       where rstrip :: String -> String
             rstrip = id

-- *ParseIni2> parseTest item "Extension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"
-- ("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi")

section :: Parser (String, String)
section = do char '['
             value <- manyTill anyChar (char ']')
             return (value, "")
          <?> "Section"

-- *ParseIni2> parseTest section "[this-is-a-section]"
-- ("this-is-a-section", "")
-- *ParseIni2> parseTest section "[not-a-section"
-- parse error at (line 1, column 15):
-- unexpected end of input
-- expecting "]"
-- *ParseIni2> parseTest section "[unfinished-a-section"
-- parse error at (line 1, column 22):
-- unexpected end of input
-- expecting "]"

line :: Parser (Maybe (String, String))
line = do skipMany space
          try (item >>= mb) <|> (section >>= mb)
       --    try (comment >> Nothing) <|> (section >>= mb) <|> (item >>= mb)
       where mb = return . Just

-- *ParseIni2> parseTest line "Extension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"
-- Just ("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi")
-- *ParseIni2> parseTest line "[Extensions]"
-- Just ("Extensions","")

fileContent :: Parser [(String, String)]
fileContent = do linesOfFile <- many line
                 (return . catMaybes) linesOfFile

fullLinesSample :: String
fullLinesSample = "[ExtensionDirs]" ++
                  "Extension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi" ++
                  "Extension1=/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi" ++
                  "Extension2=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi" ++
                  "Extension3=/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}" ++
                  "Extension4=/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com" ++
                  "Extension5=/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net" ++
                  "Extension6=/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com" ++
                  "" ++
                  "[ThemeDirs]" ++
                  "Extension0=/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}"

-- *ParseIni2> parseTest fileContent fullLinesSample
-- [("ExtensionDirs",""),("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpiExtension1=/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpiExtension2=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpiExtension3=/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}Extension4=/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.comExtension5=/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.netExtension6=/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com[ThemeDirs]Extension0=/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")]
