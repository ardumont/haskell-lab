module LoadAndUpdateIniTests where

import LoadAndUpdateIni hiding (main)
import Test.HUnit
import Text.ParserCombinators.Parsec

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
testParseEols = TestList [testParseEol1,testParseEol2,testParseEol3,testParseEol4]

testParseItem1 :: Test.HUnit.Test
testParseItem1 = ("Extension0","/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi")
                 ~=? parseRightExtract (parse item "Extension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi" "Extension0=/home/tony/.mozilla/firefox/vfazausl.default/extensions/{DDC359D1-844A-42a7-9AA1-88A850A938A8}.xpi")

testParseItems :: Test.HUnit.Test
testParseItems = TestList [testParseItem1]


testParseSection1 :: Test.HUnit.Test
testParseSection1 = ("this-is-a-section", "") ~=? parseRightExtract (parse section "[this-is-a-section]" "[this-is-a-section]")

testParseSections :: Test.HUnit.Test
testParseSections = TestList [testParseSection1]


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
testParseLines = TestList [testParseLine1, testParseLine2]

-- Full tests
tests :: Test.HUnit.Test
tests = TestList [testParseTests
                 ,testParseComments
                 ,testParseEols
                 ,testParseItems
                 ,testParseSections]

main :: IO ()
main = runTestTT tests >>= print
