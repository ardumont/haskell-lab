module ParseIni where

import Data.Ini
import Data.List

iniFilePath :: FilePath
iniFilePath = "/home/tony/.mozilla/firefox/mwad0hks.default/extensions.ini"

ioIniFile :: IO Ini
ioIniFile = do eitherIni <- readIniFile iniFilePath
               let i = case eitherIni of
                     Right x -> x
                 in return i

internWriteIniFile :: IO ()
internWriteIniFile =
  let newIniFilePath = iniFilePath ++ (show 2) in
  do iniFile <- ioIniFile
     writeIniFile newIniFilePath iniFile
