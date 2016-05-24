module TestBoard where

import           Board
import           Data.Bits
import           Masks
import           Moves
import           Test.HUnit


--- couting pieces ----
testCountPieces1 :: Test
testCountPieces1 = TestCase $ assertEqual "test count pieces 1" 12 (popCount $ whitePieces initialBoard)

testCountPieces2 :: Test
testCountPieces2 = TestCase $ assertEqual "test count pieces 2" 1 (popCount $ whitePieces Board {wp = mergeBoardFields [13, 11, 28], bp = mergeBoardFields [9, 26, 32], k = mergeBoardFields [13, 28]})

testCountPieces3 :: Test
testCountPieces3 = TestCase $ assertEqual "test count pieces 3" 3 (popCount $ blackPieces Board {wp = mergeBoardFields [13, 11, 28], bp = mergeBoardFields [9, 26, 32], k = mergeBoardFields [13, 28]})

testCountPieces4 :: Test
testCountPieces4 = TestCase $ assertEqual "test count pieces 4" 2 (popCount $ blackPieces Board {wp = mergeBoardFields [32, 31, 28], bp = mergeBoardFields [8, 14, 12], k = mergeBoardFields [14, 21]})

testCountPieces5 :: Test
testCountPieces5 = TestCase $ assertEqual "test count pieces 5" 0 (popCount $ whiteKings Board {wp = mergeBoardFields [32, 31, 28], bp = mergeBoardFields [8, 14, 12], k = mergeBoardFields [14, 21]})

testCountPieces6 :: Test
testCountPieces6 = TestCase $ assertEqual "test count pieces 6" 2 (popCount $ blackKings Board {wp = mergeBoardFields [32, 31, 28], bp = mergeBoardFields [8, 14, 12], k = mergeBoardFields [14, 12]})

testCountPieces7 :: Test
testCountPieces7 = TestCase $ assertEqual "test count pieces 7" 1 (popCount $ whiteKings Board {wp = mergeBoardFields [32, 31, 28], bp = mergeBoardFields [1,2,3,4,5,6], k = mergeBoardFields [32]})

-- making moves

testMove1:: Test
testMove1 = TestCase $ assertEqual "test move left up piece - white"
  Board {wp = mergeBoardFields [32, 18, 28], bp = mergeBoardFields [1,2,3,4,5,6], k = mergeBoardFields [32]}
  (doMove Board {wp = mergeBoardFields [32, 14, 28], bp = mergeBoardFields [1,2,3,4,5,6], k = mergeBoardFields [32]} White $ NormalMove (getBoardFields [14, 18]))

testMove2:: Test
testMove2 = TestCase $ assertEqual "test move left right piece - white"
  Board {wp = mergeBoardFields [32, 19, 28], bp = mergeBoardFields [1,2,3,4,5,6], k = mergeBoardFields [32]}
  (doMove Board {wp = mergeBoardFields [32, 25, 28], bp = mergeBoardFields [1,2,3,4,5,6], k = mergeBoardFields [32]} White $ NormalMove (getBoardFields [25, 19]))

testMove3:: Test
testMove3 = TestCase $ assertEqual "test move down left piece - black"
  Board {wp = mergeBoardFields [32, 14, 28], bp = mergeBoardFields [1,2,3,7], k = mergeBoardFields [32]}
  (doMove Board {wp = mergeBoardFields [32, 14, 28], bp = mergeBoardFields [1,2,3,11], k = mergeBoardFields [32]} Black $ NormalMove (getBoardFields [11, 7]))

testMove4:: Test
testMove4 = TestCase $ assertEqual "test move down right piece - black"
  Board {wp = mergeBoardFields [32, 19, 28], bp = mergeBoardFields [1,2,21], k = mergeBoardFields [32]}
  (doMove Board {wp = mergeBoardFields [32, 19, 28], bp = mergeBoardFields [1,2,25], k = mergeBoardFields [32]} Black $ NormalMove (getBoardFields [25, 21]))

testMove5:: Test
testMove5 = TestCase $ assertEqual "test move down left king - white"
  Board {wp = mergeBoardFields [10, 19, 28], bp = mergeBoardFields [1,2,25], k = mergeBoardFields [10]}
  (doMove Board {wp = mergeBoardFields [32, 19, 28], bp = mergeBoardFields [1,2,25], k = mergeBoardFields [32]} White $ NormalMove (getBoardFields [32, 10]))

testMove6 :: Test
testMove6 = TestCase $ assertEqual "test move down right king - white"
  Board {wp = mergeBoardFields [12, 14, 28], bp = mergeBoardFields [1,2,8], k = mergeBoardFields [12]}
  (doMove Board {wp = mergeBoardFields [19, 14, 28], bp = mergeBoardFields [1,2,8], k = mergeBoardFields [19]} White $ NormalMove (getBoardFields [19, 12]))

testMove7 :: Test
testMove7 = TestCase $ assertEqual "test move up left king - white"
  Board {wp = mergeBoardFields [22, 14, 28], bp = mergeBoardFields [1,2,8], k = mergeBoardFields [22]}
  (doMove Board {wp = mergeBoardFields [19, 14, 28], bp = mergeBoardFields [1,2,8], k = mergeBoardFields [19]} White $ NormalMove (getBoardFields [19, 22]))

testMove8 :: Test
testMove8 = TestCase $ assertEqual "test move up right king - white"
  Board {wp = mergeBoardFields [30, 14, 28], bp = mergeBoardFields [1,2,17], k = mergeBoardFields [30]}
  (doMove Board {wp = mergeBoardFields [21, 14, 28], bp = mergeBoardFields [1,2,17], k = mergeBoardFields [21]} White $ NormalMove (getBoardFields [21, 30]))

testMove9 :: Test
testMove9 = TestCase $ assertEqual "test move up left king - black"
  Board {wp = mergeBoardFields [13, 18, 2], bp = mergeBoardFields [31, 7], k = mergeBoardFields [31, 7, 2]}
  (doMove Board {wp = mergeBoardFields [13, 18, 2], bp = mergeBoardFields [24, 7], k = mergeBoardFields [24, 7, 2]} Black $ NormalMove (getBoardFields [24, 31]))

testMove10 :: Test
testMove10 = TestCase $ assertEqual "test move up right king - black"
  Board {wp = mergeBoardFields [13, 18, 2], bp = mergeBoardFields [24, 16], k = mergeBoardFields [24, 16, 2]}
  (doMove Board {wp = mergeBoardFields [13, 18, 2], bp = mergeBoardFields [24, 7], k = mergeBoardFields [24, 7, 2]} Black $ NormalMove (getBoardFields [7, 16]))

testMove11 :: Test
testMove11 = TestCase $ assertEqual "test move down right king - black"
  Board {wp = mergeBoardFields [13, 18, 2], bp = mergeBoardFields [24, 16], k = mergeBoardFields [24, 16, 2]}
  (doMove Board {wp = mergeBoardFields [13, 18, 2], bp = mergeBoardFields [24, 27], k = mergeBoardFields [24, 27, 2]} Black $ NormalMove (getBoardFields [27, 16]))

testMove12 :: Test
testMove12 = TestCase $ assertEqual "test move down left king - black"
  Board {wp = mergeBoardFields [13, 18, 3], bp = mergeBoardFields [24, 2], k = mergeBoardFields [24, 2, 3]}
  (doMove Board {wp = mergeBoardFields [13, 18, 3], bp = mergeBoardFields [24, 11], k = mergeBoardFields [24, 11, 3]} Black $ NormalMove (getBoardFields [11, 2]))

--- check promoting moves
testMove13 :: Test
testMove13 = TestCase $ assertEqual "test promoting move left - white"
  Board {wp = mergeBoardFields [29, 18, 3], bp = mergeBoardFields [24], k = mergeBoardFields [24, 3, 29]}
  (doMove Board {wp = mergeBoardFields [26, 18, 3], bp = mergeBoardFields [24], k = mergeBoardFields [24, 3]} White $ NormalMove (getBoardFields [26, 29]))

testMove14 :: Test
testMove14 = TestCase $ assertEqual "test promoting move right - white"
  Board {wp = mergeBoardFields [32, 18, 3], bp = mergeBoardFields [24], k = mergeBoardFields [24, 3, 32]}
  (doMove Board {wp = mergeBoardFields [28, 18, 3], bp = mergeBoardFields [24], k = mergeBoardFields [24, 3]} White $ NormalMove (getBoardFields [28, 32]))

testMove15 :: Test
testMove15 = TestCase $ assertEqual "test promoting move left - black"
  Board {wp = mergeBoardFields [28, 18, 3], bp = mergeBoardFields [4, 12], k = mergeBoardFields [24, 3, 4]}
  (doMove Board {wp = mergeBoardFields [28, 18, 3], bp = mergeBoardFields [7, 12], k = mergeBoardFields [24, 3]} Black $ NormalMove (getBoardFields [7, 4]))

testMove16 :: Test
testMove16 = TestCase $ assertEqual "test promoting move right - black"
  Board {wp = mergeBoardFields [28, 18, 3], bp = mergeBoardFields [2, 12], k = mergeBoardFields [24, 3, 2]}
  (doMove Board {wp = mergeBoardFields [28, 18, 3], bp = mergeBoardFields [6, 12], k = mergeBoardFields [24, 3]} Black $ NormalMove (getBoardFields [6, 2]))


-- test jumps
testMove17 :: Test
testMove17 = TestCase $ assertEqual "test piece jump left - black"
   Board {wp = mergeBoardFields [28, 18], bp = mergeBoardFields [6, 3], k = mergeBoardFields [24]}
  (doMove Board {wp = mergeBoardFields [28, 18, 7], bp = mergeBoardFields [6, 12], k = mergeBoardFields [24, 7]} Black $ JumpMove (getBoardFields [12, 3]))

testMove18 :: Test
testMove18 = TestCase $ assertEqual "test piece jump right - black"
   Board {wp = mergeBoardFields [28, 18, 7], bp = mergeBoardFields [6, 16], k = mergeBoardFields [24, 7]}
  (doMove Board {wp = mergeBoardFields [28, 18, 7, 20], bp = mergeBoardFields [6, 23], k = mergeBoardFields [24, 7]} Black $ JumpMove (getBoardFields [23, 16]))

testMove19 :: Test
testMove19 = TestCase $ assertEqual "test piece multi jump - white"
   Board {wp = field 28, bp = mergeBoardFields [6], k = field 28}
  (doMove Board {wp = field 12, bp = mergeBoardFields [15, 23, 22, 21, 13, 6, 14], k = field 12} White $ JumpMove (getBoardFields [12, 19, 26, 17, 10, 19, 28]))

testMove20 :: Test
testMove20 = TestCase $ assertEqual "test piece multi jump - white"
    Board {wp = field 32, bp = mergeBoardFields [18, 10, 11], k = 0}
  (doMove  Board {wp = field 14, bp = mergeBoardFields [18, 19, 10, 11, 28], k = 0} White $ JumpMove (getBoardFields [14, 23, 32]))

testMove21 :: Test
testMove21 = TestCase $ assertEqual "test piece multi jump - white"
  Board {wp = field 23, bp = mergeBoardFields [18, 10, 11], k = 0}
  (doMove  Board {wp = field 14, bp = mergeBoardFields [18, 19, 10, 11], k = 0} White $ JumpMove (getBoardFields [14, 23]))

testMove22 :: Test
testMove22 = TestCase $ assertEqual "test piece multi jump - white"
  Board {wp = field 28, bp = mergeBoardFields [6], k = 0}
  (doMove  Board {wp = field 12, bp = mergeBoardFields [15 , 23, 22, 21 , 13, 6, 14], k = 0} White $ JumpMove (getBoardFields [12, 19, 26, 17, 10, 19, 28]))

testMove23 :: Test
testMove23 = TestCase $ assertEqual "test piece multi jump - white"
  Board {wp = field 32, bp = 0, k = 0}
  (doMove  Board {wp = field 14, bp = mergeBoardFields[19 , 28], k = 0} White $ JumpMove (getBoardFields [14,23,32]))

testMove24 :: Test
testMove24 = TestCase $ assertEqual "test piece multi jump - white"
  Board {wp = mergeBoardFields [32, 6] , bp = 0 , k = field 32}
  (doMove  Board {wp = mergeBoardFields [1, 6] , bp = mergeBoardFields [23] , k = field 1} White $ JumpMove (getBoardFields [1,32]))

testMove25 :: Test
testMove25 = TestCase $ assertEqual "test piece multi jump - black"
  Board {bp = mergeBoardFields [14], wp = mergeBoardFields [0], k = mergeBoardFields [0]}
  (doMove  Board {bp = mergeBoardFields [5], wp = mergeBoardFields [10], k = mergeBoardFields [0]} Black $ JumpMove (getBoardFields [5,14]))

testMove26 :: Test
testMove26 = TestCase $ assertEqual "test piece multi jump - black"
  Board {bp = mergeBoardFields [1, 13], wp = mergeBoardFields [], k = mergeBoardFields [1]}
  (doMove  Board {bp = mergeBoardFields [1, 6], wp = mergeBoardFields [10], k = mergeBoardFields [1]} Black $ JumpMove (getBoardFields [6,13]))

getTestList :: [Test]
getTestList = [testCountPieces1, testCountPieces2, testCountPieces3, testCountPieces4, testCountPieces5, testCountPieces6, testCountPieces7,
               testMove1, testMove2, testMove3, testMove4, testMove5, testMove6, testMove7, testMove8, testMove9, testMove10, testMove11, testMove12,
               testMove13, testMove14, testMove15, testMove16,
               testMove17, testMove18, testMove19, testMove20, testMove21, testMove22, testMove23, testMove24, testMove25, testMove26]
