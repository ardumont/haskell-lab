module Wifi where

-- cabal module `process`
import System.Process

command :: String -> [String]
command = words

-- *Wifi> command "nmcli con list"
-- ["nmcli","con","list"]

run :: String -> IO String
run fullCommand =
  readProcess comm args []
  where (comm:args) = command fullCommand

-- *Wifi> run "nmcli con list"
-- "NAME                      UUID                                   TYPE              TIMESTAMP-REAL                    \nAndroidAP-tony            68400207-92c9-4c8f-90b4-725b45c8359f   802-11-wireless   mar. 04 f\233vr. 2014 18:44:15 CET   \nZenika-1er                076684ca-6287-4625-bab6-524b865e185e   802-11-wireless   never                             \ntatooine                  deb87d57-aedc-46a8-8994-ce83c91ce522   802-11-wireless   sam. 08 f\233vr. 2014 12:44:56 CET   \n"


main :: IO ()
main = do result <- run "nmcli con list"
          putStrLn $ "result: " ++ result

-- *Wifi> main
-- result: NAME                      UUID                                   TYPE              TIMESTAMP-REAL
-- AndroidAP-tony            68400207-92c9-4c8f-90b4-725b45c8359f   802-11-wireless   mar. 04 févr. 2014 18:44:15 CET
-- Zenika-1er                076684ca-6287-4625-bab6-524b865e185e   802-11-wireless   never
-- tatooine                  deb87d57-aedc-46a8-8994-ce83c91ce522   802-11-wireless   sam. 08 févr. 2014 12:39:56 CET
