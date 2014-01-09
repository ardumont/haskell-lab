module LoadAndUpdateIni where

--import Char
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Data.Maybe
import System.Environment

---------------- Using Parsec - http://www.serpentine.com/blog/2007/01/31/parsing-a-simple-config-file-in-haskell/

type IniConfig = Map.Map String [(String, String)]

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
eol = do try (many1 (oneOf "\r\n"))
         return ()
      <?> "eol"

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
fullLinesSample = "[ExtensionDirs]\n" ++
                  "Extension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi\n" ++
                  "Extension1=/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi\n" ++
                  "Extension2=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi\n" ++
                  "Extension3=/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}\n" ++
                  "Extension4=/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com\n" ++
                  "Extension5=/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net\n" ++
                  "Extension6=/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com\n" ++
                  "" ++
                  "[ThemeDirs]\n" ++
                  "Extension0=/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}\n"

-- *ParseIni2> parseTest fileContent fullLinesSample
-- [("ExtensionDirs",""),("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"),("Extension2","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi"),("Extension3","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}"),("Extension4","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com"),("Extension5","/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net"),("Extension6","/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com"),("ThemeDirs",""),("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")]

-- transform the current ini file into a map of section, (key, value)
mapify :: [(String, String)] -> IniConfig
mapify xs =
  internalMapify xs "" Map.empty
  where internalMapify :: [(String, String)] -> String -> IniConfig -> IniConfig
        internalMapify [] _ m = m
        internalMapify (e@(key, val) : xss) oldKey m =
          if val == ""
          then internalMapify xss key (Map.insert key [] m)
          else internalMapify xss oldKey (Map.update (\l -> Just (l ++ [e])) oldKey m) -- not performant

-- *ParseIni2> mapify [("ExtensionDirs",""),("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"), ("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"), ("ThemeDirs", ""), ("Extension0", "/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")]
-- fromList [("ExtensionDirs",[("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi")]),("ThemeDirs",[("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")])]

stringify :: IniConfig -> String
stringify m = (unlines . map stringifyLine) $ Map.keys m
              where stringifyLine k = (stringifySection k) ++ (stringifyProperties (Map.lookup k m))
                    stringifyProperties Nothing = []
                    stringifyProperties (Just xs) = (unlines . map stringifyProperty) xs
                    stringifyProperty (k, v) = k ++ "=" ++ v
                    stringifySection k = "[" ++ k ++ "]\n"

-- *ParseIni2> stringify $ mapify [("ExtensionDirs",""),("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"), ("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"), ("ThemeDirs", ""), ("Extension0", "/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")]
-- "[ExtensionDirs]\nExtension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi\nExtension1=/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi\n\n[ThemeDirs]\nExtension0=/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}\n\n"

fromString :: String -> IniConfig
fromString stringToParse =
  case (parse fileContent "" stringToParse) of
    Left _  -> Map.empty
    Right m -> mapify m

-- *ParseIni2> fromString fullLinesSample
-- fromList [("ExtensionDirs",[("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"),("Extension2","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi"),("Extension3","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}"),("Extension4","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com"),("Extension5","/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net"),("Extension6","/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com")]),("ThemeDirs",[("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")])]

fromFilePath :: FilePath -> IO IniConfig
fromFilePath filePath =
  do stringToParse <- readFile filePath
     return $ fromString stringToParse

-- *ParseIni2> fromFilePath "/home/tony/.mozilla/firefox/mwad0hks.default/extensions.ini"
-- fromList [("ExtensionDirs",[("Extension0","/home/tony/.mozilla/firefox/mwad0hks.default/extensions/anticontainer@downthemall.net.xpi"),("Extension1","/home/tony/.mozilla/firefox/mwad0hks.default/extensions/artur.dubovoy@gmail.com.xpi"),("Extension2","/home/tony/.mozilla/firefox/mwad0hks.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension3","/usr/lib/firefox/browser/extensions/{46551EC9-40F0-4e47-8E18-8E5CF550CFB8}"),("Extension4","/usr/lib/firefox/browser/extensions/mint-search-enhancer@linuxmint.com"),("Extension5","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}"),("Extension6","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com"),("Extension7","/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net"),("Extension8","/home/tony/.mozilla/firefox/mwad0hks.default/extensions/keysnail@mooz.github.com")]),("ThemeDirs",[("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")])]

countProperties :: Ord k => k -> Map.Map k [a] -> Int
countProperties k m = case Map.lookup k m of Just l -> length l

-- *ParseIni2> countProperties "ExtensionDirs" $ fromString fullLinesSample
-- 7

setProperty :: Ord k => k -> a -> Map.Map k [a] -> Map.Map k [a]
setProperty k v m = Map.update (\l -> Just (l ++ [v])) k m

-- *ParseIni2> setProperty "ExtensionDirs" ("Extension8", "test") $ fromString fullLinesSample
-- fromList [("ExtensionDirs",[("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"),("Extension2","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi"),("Extension3","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}"),("Extension4","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com"),("Extension5","/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net"),("Extension6","/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com"),("Extension8","test")]),("ThemeDirs",[("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")])]

updateNewExtension :: Map.Map String [(String, t)] -> t -> Map.Map String [(String, t)]
updateNewExtension iniProperties newExtensionValue =
  let nbExtensions = countProperties "ExtensionDirs" iniProperties
      newExtension = ("Extension" ++ (show nbExtensions), newExtensionValue)
  in setProperty "ExtensionDirs" newExtension iniProperties

-- *ParseIni2> updateNewExtension (fromString fullLinesSample) "some-value"
-- fromList [("ExtensionDirs",[("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"),("Extension2","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi"),("Extension3","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}"),("Extension4","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com"),("Extension5","/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net"),("Extension6","/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com"),("Extension7","some-value")]),("ThemeDirs",[("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")])]

loadAndUpdateExtensionsWith inputFilePath inputExtension =
  fromFilePath inputFilePath >>= \iniProperties -> return $ updateNewExtension iniProperties inputExtension

-- *ParseIni2> loadAndUpdateExtensionsWith "/home/tony/.mozilla/firefox/vfazausl.default/extensions.ini" "/home/tony/.mozilla/firefox/mwad0hks.default/extensions/keysnail@mooz.github.com"
-- input file:/home/tony/.mozilla/firefox/vfazausl.default/extensions.ini
-- extension to add:/home/tony/.mozilla/firefox/mwad0hks.default/extensions/keysnail@mooz.github.com
-- fromList [("ExtensionDirs",[("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"),("Extension2","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi"),("Extension3","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}"),("Extension4","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com"),("Extension5","/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net"),("Extension6","/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com"),("Extension7","/home/tony/.mozilla/firefox/mwad0hks.default/extensions/keysnail@mooz.github.com")]),("ThemeDirs",[("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")])]

main :: IO ()
main = do (inputFilePath:inputExtension:_) <- getArgs
          updatedIniProperties <- loadAndUpdateExtensionsWith inputFilePath inputExtension
          putStrLn $ stringify updatedIniProperties

-- ╭─tony@dagobah(0,59,) 18:11:00 ~/repo/perso/haskell-lab/src (master)
-- ╰─➤  runhaskell LoadAndUpdateIni.hs "/home/tony/.mozilla/firefox/vfazausl.default/extensions.ini" "/home/tony/.mozilla/firefox/mwad0hks.default/extensions/keysnail@mooz.github.com"
-- input file:/home/tony/.mozilla/firefox/vfazausl.default/extensions.ini
-- extension to add:/home/tony/.mozilla/firefox/mwad0hks.default/extensions/keysnail@mooz.github.com
-- [ExtensionDirs]
-- Extension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi
-- Extension1=/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi
-- Extension2=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi
-- Extension3=/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}
-- Extension4=/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com
-- Extension5=/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net
-- Extension6=/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com
-- Extension7=/home/tony/.mozilla/firefox/mwad0hks.default/extensions/keysnail@mooz.github.com

-- [ThemeDirs]
-- Extension0=/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}
