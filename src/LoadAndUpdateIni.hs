module LoadAndUpdateIni where

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

comment :: Parser ()
comment = do char '#'
             skipMany (noneOf "\r\n")
           <?> "Comment"

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

section :: Parser (String, String)
section = do char '['
             value <- manyTill anyChar (char ']')
             return (value, "")
          <?> "Section"

line :: Parser (Maybe (String, String))
line = do skipMany space
          try (item >>= mb) <|> (section >>= mb)
       --    try (comment >> Nothing) <|> (section >>= mb) <|> (item >>= mb)
       where mb = return . Just

fileContent :: Parser [(String, String)]
fileContent = many line >>= (return . catMaybes)

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

stringify :: IniConfig -> String
stringify m = (unlines . map stringifyLine) $ Map.keys m
              where stringifyLine k = (stringifySection k) ++ (stringifyProperties (Map.lookup k m))
                    stringifyProperties Nothing = []
                    stringifyProperties (Just xs) = (unlines . map stringifyProperty) xs
                    stringifyProperty (k, v) = k ++ "=" ++ v
                    stringifySection k = "[" ++ k ++ "]\n"

fromString :: String -> IniConfig
fromString stringToParse =
  case (parse fileContent "" stringToParse) of
    Left _  -> Map.empty
    Right m -> mapify m

fromFilePath :: FilePath -> IO IniConfig
fromFilePath filePath =
  do stringToParse <- readFile filePath
     return $ fromString stringToParse

countProperties :: Ord k => k -> Map.Map k [a] -> Int
countProperties k m = case Map.lookup k m of Just l -> length l

setProperty :: Ord k => k -> a -> Map.Map k [a] -> Map.Map k [a]
setProperty k v m = Map.update (\l -> Just (l ++ [v])) k m

updateNewExtension :: IniConfig -> String -> IniConfig
updateNewExtension iniProperties newExtensionValue =
  let nbExtensions = countProperties "ExtensionDirs" iniProperties
      newExtension = ("Extension" ++ (show nbExtensions), newExtensionValue)
  in setProperty "ExtensionDirs" newExtension iniProperties

loadAndUpdateExtensionsWith :: FilePath -> String -> IO IniConfig
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
