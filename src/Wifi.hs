module Wifi where

-----------------------------------------------------------------------------
-- |
-- Module      :  Wifi
-- Copyright   :  (c) Commiters
-- License     :  The same as `nmcli` - http://manpages.ubuntu.com/manpages/maverick/man1/nmcli.1.html
--
-- Maintainer  :  ardumont
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple module to deal with wifi connections.
-- At the moment, only connection to a wifi with autoconnect policy.
--
-- Use: runhaskell Wifi.hs
-----------------------------------------------------------------------------


-- cabal module `process`
import System.Process
import qualified Data.Map as Map
import Data.Function (on)
import Data.List (sortBy)

commandListWifiAutoconnect :: String
commandListWifiAutoconnect = "nmcli --terse --fields ssid,signal dev wifi"

commandScanWifi :: String
commandScanWifi = "nmcli --terse --fields name con list"

command :: String -> [String]
command = words

run :: String -> IO [String]
run fullCommand =
  do result <- readProcess comm args []
     return $ lines result
  where (comm:args) = command fullCommand

-- *Wifi> run "nmcli --terse --fields name con list"
-- ["myrkr","dantooine","tatooine"]
-- *Wifi> run "nmcli con list"
-- ["NAME                      UUID                                   TYPE              TIMESTAMP-REAL                    ","dantooine            68400207-92c9-4c8f-90b4-725b45c8359f   802-11-wireless   mar. 04 f\233vr. 2014 18:44:15 CET   ","myrkr                076684ca-6287-4625-bab6-524b865e185e   802-11-wireless   never                             ","tatooine                  deb87d57-aedc-46a8-8994-ce83c91ce522   802-11-wireless   sam. 08 f\233vr. 2014 13:04:56 CET   "]

-- Scan the wifi and return the ssid:signal
-- *Wifi> run "nmcli --terse --fields ssid,signal dev wifi"
-- ["'Livebox-0ff6':42","'tatooine':72"]

cleanString :: String -> String
cleanString s =
  if (elem '\'' s)
  then tail . init $ s
  else s

sliceSSIDSignal :: String -> (String, String)
sliceSSIDSignal s =
  (cleanString ssid, tail signal)
  where (ssid, signal) = break (== ':') s

sliceSSIDSignals :: [String] -> [(String, String)]
sliceSSIDSignals = map sliceSSIDSignal

scanWifi :: IO (Map.Map String String)
scanWifi =
  fmap (Map.fromList . map sliceSSIDSignal) $ run commandListWifiAutoconnect

-- *Wifi> scanWifi
-- fromList [("Livebox-0ff6","42"),("freewifi","75")]

listAutoConnectWifi :: IO [String]
listAutoConnectWifi = run commandScanWifi

-- *Wifi> listAutoConnectWifi
-- ["dantooine","myrkr","tatooine"]

-- Filter the list of wifis the machine (in its current setup) can autoconnect to
wifiToConnect :: Ord k => [k] -> Map.Map k a -> [k]
wifiToConnect autoConnectWifis scannedWifis =
  filter (flip Map.member scannedWifis) autoConnectWifis

connectToWifiCommand :: String -> String
connectToWifiCommand wifi = "nmcli con up id " ++ wifi

-- elect wifi according to signal's power (the more powerful is elected)
electWifi :: [String] -> Map.Map String String -> String
electWifi [w] _ = w
electWifi wifis scannedWifis =
  (fst . head . sortBy (compare `on` snd) . map filteredWifiCouple) wifis
  where filteredWifiCouple wifi = (wifi, w)
          where Just w = Map.lookup wifi scannedWifis

-- Scan the wifi, compute the list of autoconnect wifis, connect to one (if multiple possible, the one with the most powerful signal is elected)
main :: IO ()
main = do scannedWifis <- scanWifi
          autoConnectWifis <- listAutoConnectWifi
          (run . connectToWifiCommand . flip electWifi scannedWifis . wifiToConnect autoConnectWifis) scannedWifis
          putStrLn $ "Connecting to wifi done!"
