module WifiTests where

import Wifi (command, cleanString, sliceSSIDSignal, sliceSSIDSignals)
import Test.HUnit

testCommand1 :: Test.HUnit.Test
testCommand1 = ["nmcli","con","list"] ~=? command "nmcli con list"

testCommand2 :: Test.HUnit.Test
testCommand2 = ["nmcli", "-t", "-f", "name", "con","list"] ~=? command "nmcli -t -f name con list"

testCommands :: Test.HUnit.Test
testCommands = TestList ["testCommand1" ~: testCommand1, "testCommand2" ~: testCommand2]

testCleanString1 :: Test.HUnit.Test
testCleanString1 = "hello" ~=? cleanString "'hello'"

testCleanString2 :: Test.HUnit.Test
testCleanString2 = "hell" ~=? cleanString "'hello"

testCleanString3 :: Test.HUnit.Test
testCleanString3 = "hello" ~=? cleanString "hello"

testCleanString4 :: Test.HUnit.Test
testCleanString4 = "ello" ~=? cleanString "hello'"

testCleanStrings :: Test.HUnit.Test
testCleanStrings = TestList ["testCleanString1" ~: testCleanString1
                             ,"testCleanString2" ~: testCleanString2
                             ,"testCleanString3" ~: testCleanString3
                             ,"testCleanString4" ~: testCleanString4]

testSliceSSIDSignal1 :: Test.HUnit.Test
testSliceSSIDSignal1 = ("ssid","signal") ~=? sliceSSIDSignal "ssid:signal"

testSliceSSIDSignal2 :: Test.HUnit.Test
testSliceSSIDSignal2 = ("ssid", "signal") ~=? sliceSSIDSignal "'ssid':signal"

testSliceSSIDSignals :: Test.HUnit.Test
testSliceSSIDSignals = TestList ["testSliceSSIDSignal1" ~: testSliceSSIDSignal1
                                ,"testSliceSSIDSignal2" ~: testSliceSSIDSignal2]

testSliceSSIDSignals1 :: Test.HUnit.Test
testSliceSSIDSignals1 = [("Livebox-0ff6","42"),("tatooine","71")]
                       ~=?
                       sliceSSIDSignals ["'Livebox-0ff6':42","'tatooine':71"]


testSliceSSIDSignalss :: Test.HUnit.Test
testSliceSSIDSignalss = TestList ["testSliceSSIDSignals1" ~: testSliceSSIDSignals1]

-- Full tests
tests :: Test.HUnit.Test
tests = TestList [testCommands
                  ,testCleanStrings
                  ,testSliceSSIDSignals
                  ,testSliceSSIDSignalss]

main :: IO ()
main = runTestTT tests >>= print
