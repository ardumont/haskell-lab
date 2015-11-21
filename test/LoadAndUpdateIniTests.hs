module LoadAndUpdateIniTests where

import qualified Data.Map                      as Map
import           LoadAndUpdateIni              hiding (main)
import           Test.HUnit
import           Text.ParserCombinators.Parsec

parseRightExtract :: Either t t1 -> t1
parseRightExtract (Right v) = v
parseRightExtract (Left _) = error "Not expected"

parseLeftExtract :: Either t t1 -> t
parseLeftExtract (Right _) = error "Not expected!"
parseLeftExtract (Left v) = v

testParseTest1 :: Test.HUnit.Test
testParseTest1 = "ab" ~=? parseRightExtract (parse ident "ab" "ab")

testParseTest2 :: Test.HUnit.Test
testParseTest2 = "abc1" ~=? parseRightExtract (parse ident "abc1" "abc1")

testParseTest3 :: Test.HUnit.Test
testParseTest3 = "abc1234" ~=? parseRightExtract (parse ident "abc1234" "abc1234")

-- testParseTest4 :: Test.HUnit.Test
-- testParseTest4 = error "Not expected!" ~=? parseLeftExtract (parse ident "abc1234" "something-else")

-- *ParseIni2> parseTest ident "123"
-- parse error at (line 1, column 1):
-- unexpected "1"
-- expecting Identifier

testParseTests :: Test.HUnit.Test
testParseTests = TestList ["testParseTest1" ~: testParseTest1
                           ,"testParseTest2" ~: testParseTest2
                           ,"testParseTest3" ~: testParseTest3]

testParseComment1 :: Test.HUnit.Test
testParseComment1 = () ~=? parseRightExtract (parse comment "# this is a comment" "# this is a comment")

-- *ParseIni2> parseTest comment "this is not a comment"
-- parse error at (line 1, column 1):
-- unexpected "t"
-- expecting Comment

testParseComments :: Test.HUnit.Test
testParseComments = TestList ["testParseComment1" ~: testParseComment1]

testParseEol1 :: Test.HUnit.Test
testParseEol1 = () ~=? parseRightExtract (parse eol "\n" "\n")

testParseEol2 :: Test.HUnit.Test
testParseEol2 = () ~=? parseRightExtract (parse eol "\r\n" "\r\n")

testParseEol3 :: Test.HUnit.Test
testParseEol3 = () ~=? parseRightExtract (parse eol "\r" "\r")

testParseEol4 :: Test.HUnit.Test
testParseEol4 = () ~=? parseRightExtract (parse eol "\n\r" "\n\r")


-- *LoadAndUpdateIni> parseTest  eol "some-other"
-- parse error at (line 1, column 1):
-- unexpected "s"
-- expecting eol

testParseEols :: Test.HUnit.Test
testParseEols = TestList ["testParseEol1" ~: testParseEol1
                         ,"testParseEol2" ~: testParseEol2
                         ,"testParseEol3" ~: testParseEol3
                         ,"testParseEol4" ~: testParseEol4]

testParseItem1 :: Test.HUnit.Test
testParseItem1 = ("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi")
                 ~=? parseRightExtract (parse item "Extension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi" "Extension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi")

testParseItems :: Test.HUnit.Test
testParseItems = TestList ["testParseItem1" ~: testParseItem1]


testParseSection1 :: Test.HUnit.Test
testParseSection1 = ("this-is-a-section", "") ~=? parseRightExtract (parse section "[this-is-a-section]" "[this-is-a-section]")

testParseSections :: Test.HUnit.Test
testParseSections = TestList ["testParseSection1" ~: testParseSection1]


-- *ParseIni2> parseTest section "[not-a-section"
-- parse error at (line 1, column 15):
-- unexpected end of input
-- expecting "]"
-- *ParseIni2> parseTest section "[unfinished-a-section"
-- parse error at (line 1, column 22):
-- unexpected end of input
-- expecting "]"

testParseLine1 :: Test.HUnit.Test
testParseLine1 = Just ("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi")
                 ~=?
                 parseRightExtract (parse line "Extension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi" "Extension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi")

testParseLine2 :: Test.HUnit.Test
testParseLine2 = Just ("Extensions","") ~=? parseRightExtract (parse line "[Extensions]" "[Extensions]")

testParseLines :: Test.HUnit.Test
testParseLines = TestList ["testParseLine1" ~: testParseLine1
                          ,"testParseLine2" ~: testParseLine2]

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

testParseFileContent1 :: Test.HUnit.Test
testParseFileContent1 = [("ExtensionDirs",""),("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"),("Extension2","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi"),("Extension3","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}"),("Extension4","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com"),("Extension5","/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net"),("Extension6","/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com"),("ThemeDirs",""),("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")]
                       ~=? parseRightExtract (parse fileContent fullLinesSample fullLinesSample)

testParseFileContents :: Test.HUnit.Test
testParseFileContents = TestList ["testParseFileContent1" ~: testParseFileContent1]

testMapify1 :: Test.HUnit.Test
testMapify1 = Map.fromList [("ExtensionDirs",[("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi")]),("ThemeDirs",[("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")])]
             ~=?
             mapify [("ExtensionDirs",""),("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"), ("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"), ("ThemeDirs", ""), ("Extension0", "/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")]

testMapifys :: Test.HUnit.Test
testMapifys = TestList ["testMapify1" ~: testMapify1]

testStringify1 :: Test.HUnit.Test
testStringify1 = "[ExtensionDirs]\nExtension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi\nExtension1=/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi\nExtension2=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi\nExtension3=/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}\nExtension4=/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com\nExtension5=/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net\nExtension6=/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com\n\n[ThemeDirs]\nExtension0=/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}\n\n"
                 ~=?
                 stringify (Map.fromList [("ExtensionDirs",[("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"),("Extension2","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi"),("Extension3","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}"),("Extension4","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com"),("Extension5","/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net"),("Extension6","/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com")]),("ThemeDirs",[("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")])])

testStringifys :: Test.HUnit.Test
testStringifys = TestList ["testStringify1" ~: testStringify1]

testFromString1 :: Test.HUnit.Test
testFromString1 = Map.fromList [("ExtensionDirs",[("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"),("Extension2","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi"),("Extension3","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}"),("Extension4","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com"),("Extension5","/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net"),("Extension6","/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com")]),("ThemeDirs",[("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")])]
                  ~=?
                  fromString fullLinesSample

testFromStrings :: Test.HUnit.Test
testFromStrings = TestList ["testFromString1" ~: testFromString1]

-- testFromFilePath1 :: Test.HUnit.Test
-- testFromFilePath1 = Map.fromList [("ExtensionDirs",[("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"),("Extension2","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi"),("Extension3","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}"),("Extension4","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com"),("Extension5","/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net"),("Extension6","/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com")]),("ThemeDirs",[("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")])]
--                     ~=?
--                     fromFilePath "../resources/extensions.ini"

-- testFromFilePaths :: Test.HUnit.Test
-- testFromFilePaths = TestList [testFromFilePath1]


testCountProperties1 :: Test.HUnit.Test
testCountProperties1 = 7
                       ~=?
                       countProperties "ExtensionDirs" (fromString fullLinesSample)

testCountPropertiess :: Test.HUnit.Test
testCountPropertiess = TestList ["testCountProperties1" ~: testCountProperties1]

testSetProperty1 :: Test.HUnit.Test
testSetProperty1 = Map.fromList [("ExtensionDirs",[("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"),("Extension2","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi"),("Extension3","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}"),("Extension4","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com"),("Extension5","/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net"),("Extension6","/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com"),("Extension8","test")]),("ThemeDirs",[("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")])]
                  ~=?
                  setProperty "ExtensionDirs" ("Extension8", "test") (fromString fullLinesSample)

testSetProperties :: Test.HUnit.Test
testSetProperties = TestList ["testSetProperty1" ~: testSetProperty1]


testUpdateNewExtension1 :: Test.HUnit.Test
testUpdateNewExtension1 = Map.fromList [("ExtensionDirs",[("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi"),("Extension1","/home/tony/.mozilla/firefox/vfazausl.default/extensions/artur.dubovoy@gmail.com.xpi"),("Extension2","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{a3a5c777-f583-4fef-9380-ab4add1bc2a2}.xpi"),("Extension3","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/{2e1445b0-2682-11e1-bfc2-0800200c9a66}"),("Extension4","/usr/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/ubufox@ubuntu.com"),("Extension5","/usr/lib/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}/online-accounts@lists.launchpad.net"),("Extension6","/home/tony/.mozilla/firefox/vfazausl.default/extensions/keysnail@mooz.github.com"),("Extension7","some-value")]),("ThemeDirs",[("Extension0","/usr/lib/firefox/browser/extensions/{972ce4c6-7e08-4474-a285-3208198ce6fd}")])]
                          ~=?
                          updateNewExtension (fromString fullLinesSample) "some-value"


wtestUpdateNewExtensions :: Test.HUnit.Test
wtestUpdateNewExtensions = TestList ["testUpdateNewExtension1" ~: testUpdateNewExtension1]

-- Full tests
tests :: [Test]
tests = [testParseTests
        ,testParseComments
        ,testParseEols
        ,testParseItems
        ,testParseSections
        ,testParseFileContents
        ,testMapifys
        ,testStringifys
        ,testFromStrings
        ,testCountPropertiess
        ,testSetProperties
        ,wtestUpdateNewExtensions]

runTests :: IO ()
runTests = runTestTT (TestList tests) >>= print
