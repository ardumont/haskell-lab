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

-- Full tests
tests :: Test.HUnit.Test
tests = TestList [testParseTests]

main :: IO ()
main = runTestTT tests >>= print
