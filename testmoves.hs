module TestMoves where

import           Board
import           Masks
import           Moves
import           Test.HUnit

-- test detection movers

testMovers1 :: Test
testMovers1 = TestCase $ assertEqual "Test white movers for empty board" (getWhiteMovers Board {bp = 0, wp = 0, k = 0}) 0

testMovers2 :: Test
testMovers2 = TestCase $ assertEqual "Test white movers for initial board" (getWhiteMovers initialBoard) (mergeBoardFields [9, 10, 11, 12])

testMovers3 :: Test
testMovers3 = TestCase $ assertEqual "Test white movers 3"
                        (getWhiteMovers Board {wp = mergeBoardFields [25, 1, 24], bp = mergeBoardFields [9, 5, 2], k = field 1}) (mergeBoardFields [25, 24])

testMovers4 :: Test
testMovers4 = TestCase $ assertEqual "Test white movers 4"
                        (getWhiteMovers Board {wp = mergeBoardFields [4, 30, 25, 1, 24], bp = mergeBoardFields [9, 10, 2], k = field 1}) (mergeBoardFields [25, 24, 1, 4])

testMovers5 :: Test
testMovers5 = TestCase $ assertEqual "Test white movers 5"
                        (getWhiteMovers Board {wp = mergeBoardFields [4, 29, 25, 1, 24], bp = mergeBoardFields [9, 10, 2], k = field 1}) (mergeBoardFields [24, 1, 4])

testJumpers1 :: Test
testJumpers1 = TestCase $ assertEqual "Test white jumpers 1" (getWhiteJumpers Board {bp = field 28, wp = field 5, k = field 5}) (field 5)

testJumpers2 :: Test
testJumpers2 = TestCase $ assertEqual "Test white jumpers 2" (getWhiteJumpers Board {bp = field 5, wp = field 28, k = field 28}) (field 28)

testJumpers3 :: Test
testJumpers3 = TestCase $ assertEqual "Test white jumpers 3" (getWhiteJumpers Board {bp = field 12, wp = field 15, k = field 15}) (field 15)

testJumpers4 :: Test
testJumpers4 = TestCase $ assertEqual "Test white jumpers 4" (getWhiteJumpers Board {bp = field 19, wp = field 8, k = field 8}) (field 8)


testJumpers5 :: Test
testJumpers5 = TestCase $ assertEqual "Test white jumpers 5" (getWhiteJumpers Board {bp = field 29, wp = field 8, k = field 8}) 0

testJumpers6 :: Test
testJumpers6 = TestCase $ assertEqual "Test white jumpers 6" (getWhiteJumpers Board {bp = field 26, wp = mergeBoardFields [21, 17], k = field 17}) (field 21)

testJumpers7 :: Test
testJumpers7 = TestCase $ assertEqual "Test white jumpers 7" (getWhiteJumpers Board {bp = mergeBoardFields [19, 23], wp = mergeBoardFields [1, 14], k = mergeBoardFields [1, 14]}) 0

testJumpers8 :: Test
testJumpers8 = TestCase $ assertEqual "Test white jumpers 8" (getWhiteJumpers Board {bp = mergeBoardFields [19, 28], wp = mergeBoardFields [10, 14], k = mergeBoardFields [10, 14]}) (field 14)

testJumpers9 :: Test
testJumpers9 = TestCase $ assertEqual "Test white jumpers 9" (getWhiteJumpers Board {bp = mergeBoardFields [21, 19, 28], wp = mergeBoardFields [10, 14], k = mergeBoardFields [10, 14]}) (field 14)

testJumpers10 :: Test
testJumpers10 = TestCase $ assertEqual "Test white jumpers 10" (getWhiteJumpers Board {bp = mergeBoardFields [13, 21, 19, 28], wp = mergeBoardFields [10, 14], k = mergeBoardFields [10, 14]}) (mergeBoardFields [14, 10])

testJumpers11 :: Test
testJumpers11 = TestCase $ assertEqual "Test white jumpers 11" (getWhiteJumpers Board {bp = mergeBoardFields [7, 5, 12, 13, 21, 19, 28],
                                                                                       wp = mergeBoardFields [11, 2, 10, 14 , 29],
                                                                                       k = mergeBoardFields [10, 14, 29]}) (mergeBoardFields [14, 10, 29, 2, 11])

testJumpers12 :: Test
testJumpers12 = TestCase $ assertEqual "Test white jumpers 12" (getWhiteJumpers Board {bp = mergeBoardFields [28, 19, 10], wp = field 32, k = field 32}) (field 32)


getTestList :: [Test]
getTestList = [testMovers1, testMovers2, testMovers3, testMovers4, testMovers5,
                        testJumpers1, testJumpers2, testJumpers3, testJumpers4, testJumpers5, testJumpers6, testJumpers7, testJumpers8, testJumpers9, testJumpers10, testJumpers11, testJumpers12]
