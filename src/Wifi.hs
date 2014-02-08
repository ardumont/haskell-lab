module Wifi where

-- cabal module `process`
import System.Process

command :: String -> [String]
command = words

-- *Wifi> command "nmcli con list"
-- ["nmcli","con","list"]

run :: String -> IO [String]
run fullCommand =
  do result <- readProcess comm args []
     return $ lines result
  where (comm:args) = command fullCommand

-- *Wifi> run "nmcli --terse --fields name con list"
-- ["AndroidAP-tony","Zenika-1er","tatooine"]
-- *Wifi> run "nmcli con list"
-- ["NAME                      UUID                                   TYPE              TIMESTAMP-REAL                    ","AndroidAP-tony            68400207-92c9-4c8f-90b4-725b45c8359f   802-11-wireless   mar. 04 f\233vr. 2014 18:44:15 CET   ","Zenika-1er                076684ca-6287-4625-bab6-524b865e185e   802-11-wireless   never                             ","tatooine                  deb87d57-aedc-46a8-8994-ce83c91ce522   802-11-wireless   sam. 08 f\233vr. 2014 13:04:56 CET   "]

main :: IO ()
main = do result <- run "nmcli con list"
          mapM_ putStrLn result

-- *Wifi> main
-- NAME                      UUID                                   TYPE              TIMESTAMP-REAL
-- AndroidAP-tony            68400207-92c9-4c8f-90b4-725b45c8359f   802-11-wireless   mar. 04 févr. 2014 18:44:15 CET
-- Zenika-1er                076684ca-6287-4625-bab6-524b865e185e   802-11-wireless   never
-- tatooine                  deb87d57-aedc-46a8-8994-ce83c91ce522   802-11-wireless   sam. 08 févr. 2014 13:04:56 CET
