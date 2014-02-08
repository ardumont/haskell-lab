module WifiTest where

import Wifi (command, cleanString)
import Test.HUnit

testCommand1 :: Test.HUnit.Test
testCommand1 = ["nmcli","con","list"] ~=? command "nmcli con list"

testCommand2 :: Test.HUnit.Test
testCommand2 = ["nmcli", "-t", "-f", "con","list"] ~=? command "nmcli -t -f name con list"

testCommands :: Test.HUnit.Test
testCommands = TestList ["testCommand1" ~: testCommand1, "testCommand2" ~: testCommand2]

testCleanString1 :: Test.HUnit.Test
testCleanString1 = "hello" ~=? cleanString "'hello'"

testCleanString2 :: Test.HUnit.Test
testCleanString2 = "hell" ~=? cleanString "'hello"

testCleanString3 :: Test.HUnit.Test
testCleanString3 = "hello" ~=? cleanString "hello"

testCleanStrings :: Test.HUnit.Test
testCleanStrings = TestList ["testCleanString1" ~: testCleanString1, "testCleanString2" ~: testCleanString2]

-- Full tests
tests :: Test.HUnit.Test
tests = TestList [testCommands,
                  testCleanStrings]

main :: IO ()
main = runTestTT tests >>= print
