module TestMoves where

import           Board
import           Data.Bits
import           Masks
import           Moves
import           Test.HUnit

-- test detection movers

testMovers1 :: Test
testMovers1 = TestCase $ assertEqual "Test white movers for empty board" (getWhiteMovers Board {bp = 0, wp = 0, k = 0}) 0

testMovers2 :: Test
testMovers2 = TestCase $ assertEqual "Test white movers for initial board" (getWhiteMovers initialBoard) (field_9 .|. field_10 .|. field_11 .|. field_12)

testMovers3 :: Test
testMovers3 = TestCase $ assertEqual "Test white movers 3" 
                        (getWhiteMovers Board {wp = field_25 .|. field_1 .|. field_24, bp = field_9 .|. field_5 .|. field_2, k = field_1}) 
                        (field_25 .|. field_24)

testMovers4 :: Test
testMovers4 = TestCase $ assertEqual "Test white movers 4" 
                        (getWhiteMovers Board {wp = field_4 .|. field_30 .|. field_25 .|. field_1 .|. field_24, bp = field_9 .|. field_10 .|. field_2, k = field_1}) 
                        (field_25 .|. field_24 .|. field_1 .|. field_4)

testMovers5 :: Test
testMovers5 = TestCase $ assertEqual "Test white movers 5" 
                        (getWhiteMovers Board {wp = field_4 .|. field_29 .|. field_25 .|. field_1 .|. field_24, bp = field_9 .|. field_10 .|. field_2, k = field_1}) 
                        (field_24 .|. field_1 .|. field_4)

testJumpers1 :: Test
testJumpers1 = TestCase $ assertEqual "Test white jumpers 1" (getWhiteJumpers Board {bp = field_28, wp = field_5, k = field_5}) field_5

testJumpers2 :: Test
testJumpers2 = TestCase $ assertEqual "Test white jumpers 2" (getWhiteJumpers Board {bp = field_5, wp = field_28, k = field_28}) field_28

testJumpers3 :: Test
testJumpers3 = TestCase $ assertEqual "Test white jumpers 3" (getWhiteJumpers Board {bp = field_12, wp = field_15, k = field_15}) field_15

testJumpers4 :: Test
testJumpers4 = TestCase $ assertEqual "Test white jumpers 4" (getWhiteJumpers Board {bp = field_19, wp = field_8, k = field_8}) field_8


testJumpers5 :: Test
testJumpers5 = TestCase $ assertEqual "Test white jumpers 5" (getWhiteJumpers Board {bp = field_29, wp = field_8, k = field_8}) 0

testJumpers6 :: Test
testJumpers6 = TestCase $ assertEqual "Test white jumpers 6" (getWhiteJumpers Board {bp = field_26, wp = field_21 .|. field_17, k = field_17}) field_21

testJumpers7 :: Test
testJumpers7 = TestCase $ assertEqual "Test white jumpers 7" (getWhiteJumpers Board {bp = field_19 .|. field_23, wp = field_1 .|. field_14, k = field_1 .|. field_14}) 0

testJumpers8 :: Test
testJumpers8 = TestCase $ assertEqual "Test white jumpers 8" (getWhiteJumpers Board {bp = field_19 .|. field_28, wp = field_10 .|. field_14, k = field_10 .|. field_14}) field_14

testJumpers9 :: Test
testJumpers9 = TestCase $ assertEqual "Test white jumpers 9" (getWhiteJumpers Board {bp = field_21 .|. field_19 .|. field_28, wp = field_10 .|. field_14, k = field_10 .|. field_14}) field_14

testJumpers10 :: Test
testJumpers10 = TestCase $ assertEqual "Test white jumpers 10" (getWhiteJumpers Board {bp = field_13 .|. field_21 .|. field_19 .|. field_28, wp = field_10 .|. field_14, k = field_10 .|. field_14}) (field_14 .|. field_10)

testJumpers11 :: Test
testJumpers11 = TestCase $ assertEqual "Test white jumpers 11" (getWhiteJumpers Board {bp = field_7 .|. field_5 .|. field_12 .|. field_13 .|. field_21 .|. field_19 .|. field_28, 
                                                                                       wp = field_11 .|. field_2 .|. field_10 .|. field_14  .|. field_29, 
                                                                                       k = field_10 .|. field_14 .|. field_29}) (field_14 .|. field_10 .|. field_29 .|. field_2 .|. field_11)

testJumpers12 :: Test
testJumpers12 = TestCase $ assertEqual "Test white jumpers 12" (getWhiteJumpers Board {bp = field_28 .|. field_19 .|. field_10, wp = field_32, k = field_32}) field_32


getTestList = [testMovers1, testMovers2, testMovers3, testMovers4, testMovers5,
                        testJumpers1, testJumpers2, testJumpers3, testJumpers4, testJumpers5, testJumpers6, testJumpers7, testJumpers8, testJumpers9, testJumpers10, testJumpers11, testJumpers12]
