module TestMovesListBlack where

import           Board
import           Masks
import           Moves
import           Test.HUnit

import           Data.List
import           Data.Word

assertBoolLists :: [[Word64]] -> [[Word64]] -> Bool
assertBoolLists x y = sort x == sort y

testMoveList1 :: Test
testMoveList1 = TestCase $ assertBool "test move list 1"  $ assertBoolLists (getBlackMovesList initialBoard)
    (map getBoardFields  [[21, 17],[21, 18],[22, 18],
    [22, 19],[23, 19],[23, 20],[24, 20]])

testMoveList2 :: Test
testMoveList2 = TestCase $ assertBool "test move list 2"  $ assertBoolLists (getBlackMovesList Board {bp = mergeBoardFields [32], wp = mergeBoardFields [0], k = mergeBoardFields [32]})
    (map getBoardFields  [[32, 5],[32, 10],[32, 14],[32, 19],[32, 23],[32, 28],[32, 1]])

testMoveList3 :: Test
testMoveList3 = TestCase $ assertBool "test move list 3"  $ assertBoolLists (getBlackMovesList Board {bp = mergeBoardFields [14], wp = mergeBoardFields [0], k = mergeBoardFields [14]})
    (map getBoardFields  [[14, 5],[14, 10],[14, 1],[14, 19],[14, 23],[14, 28],[14, 32],[14, 18],[14, 21],[14, 25],[14, 11], [14, 7],[14, 4]])

testMoveList4 :: Test
testMoveList4 = TestCase $ assertBool "test move list 4"  $ assertBoolLists (getBlackMovesList Board {bp = mergeBoardFields [9, 21, 7], wp = mergeBoardFields [0], k = mergeBoardFields [21]})
    (map getBoardFields  [[9, 5],[21, 25],[21, 18],[21, 14],[21, 11],[7, 3],[7, 4],[21, 17],[21, 26],[21, 30]])

testMoveList5 :: Test
testMoveList5 = TestCase $ assertBool "test move list 5"  $ assertBoolLists (getBlackMovesList Board {bp = mergeBoardFields [27, 20, 15], wp = mergeBoardFields [23], k = mergeBoardFields [20]})
    (map getBoardFields  [[27, 22],[15, 11],[15, 12],[20, 24],[20, 16]])

testMoveList6 :: Test
testMoveList6 = TestCase $ assertBool "test move list 6"  $ assertBoolLists (getBlackMovesList Board {bp = mergeBoardFields [21, 18, 14], wp = mergeBoardFields [11, 7, 10, 5], k = mergeBoardFields [14]})
    (map getBoardFields  [[21, 17],[18, 13],[14, 19],[14, 23],[14, 28],[14, 32]])



-------- PIECES JUMPS ---------------
testJumpList1 :: Test
testJumpList1 = TestCase $ assertBool "test jump list 1"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [14], wp = mergeBoardFields [18, 19, 10, 11], k = mergeBoardFields [0]})
        (map getBoardFields [[14, 21],[14, 23],[14, 5], [14, 7]])

testJumpList2 :: Test
testJumpList2 = TestCase $ assertBool "test jump list 2"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [5], wp = mergeBoardFields [10], k = mergeBoardFields [0]})
        (map getBoardFields [[5, 14]])

testJumpList3 :: Test
testJumpList3 = TestCase $ assertBool "test jump list 3"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [14], wp = mergeBoardFields [18, 19, 10, 11, 28], k = mergeBoardFields [0]})
        (map getBoardFields [[14, 21],[14, 23, 32],[14, 5], [14, 7]])

testJumpList4 :: Test
testJumpList4 = TestCase $ assertBool "test jump list 4"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [12], wp = mergeBoardFields [15 , 23, 22, 21 , 13, 6, 14], k = mergeBoardFields [0]})
        (map getBoardFields [[12, 19, 28], [12, 19, 26, 17, 10, 3],[12, 19, 26, 17, 10, 19, 28], [12, 19, 10, 3],[12, 19, 10, 17, 26, 19, 28]])


testJumpList5 :: Test
testJumpList5 = TestCase $ assertBool "test jump list 5"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [14], wp = mergeBoardFields [19 , 28], k = mergeBoardFields [0]})
        (map getBoardFields [[14,23,32]])

testJumpList6 :: Test
testJumpList6 = TestCase $ assertBool "test jump list 6"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [19, 28], wp = mergeBoardFields [14 , 23], k = mergeBoardFields [0]})
        (map getBoardFields [[19,10]])

-------------- KINGS JUMPS -------------
testJumpKingsList1 :: Test
testJumpKingsList1 = TestCase $ assertBool "test jump king list 1"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [10], wp = mergeBoardFields [14], k = mergeBoardFields [10]})
           (map getBoardFields [[10,19], [10,23],[10,28], [10,32]])


testJumpKingsList2 :: Test
testJumpKingsList2 = TestCase $ assertBool "test jump king list 2"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [10], wp = mergeBoardFields [28], k = mergeBoardFields [10]})
           (map getBoardFields [[10,32]])


testJumpKingsList3 :: Test
testJumpKingsList3 = TestCase $ assertBool "test jump king list 3"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [14], wp = mergeBoardFields [18, 19, 10, 11], k = mergeBoardFields [14]})
        (map getBoardFields [[14, 21],[14, 25],[14, 23],[14, 28], [14, 32],[14, 5], [14, 1],[14, 7], [14, 4]])

testJumpKingsList4 :: Test
testJumpKingsList4 = TestCase $ assertBool "test jump king list 4"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [14], wp = mergeBoardFields [18, 19, 10, 11, 28], k = mergeBoardFields [14]})
        (map getBoardFields [[14, 21],[14, 25],[14, 23,32],[14, 5], [14, 1],[14, 7], [14, 4]])

testJumpKingsList5 :: Test
testJumpKingsList5 = TestCase $ assertBool "test jump king list 5"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [12], wp = mergeBoardFields [15 , 23, 22, 21 , 13, 6, 14], k = mergeBoardFields [12]})
        (map getBoardFields [[12, 19, 28], [12, 19, 32],
        [12, 19, 26, 17, 10, 3],
        [12, 19, 26, 17, 10, 19, 28], [12, 19, 26, 17, 10, 19, 32],
        [12, 19, 10, 3],
        [12, 19, 10, 17, 26, 19, 28],
        [12, 19, 10, 17, 30, 20],
        [12, 19, 10, 17, 30, 16],
        [12, 19, 10, 17, 26, 19, 32]])


testJumpKingsList6 :: Test
testJumpKingsList6 = TestCase $ assertBool "test jump king list 6"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [14], wp = mergeBoardFields [19 , 28], k = mergeBoardFields [14]})
        (map getBoardFields [[14,23,32]])


------- KINGS AND PIECES JUMPS -------------

testJumpKingsPiecesList1 :: Test
testJumpKingsPiecesList1 = TestCase $ assertBool "test jump king pieces list 1"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [1, 6 ], wp = mergeBoardFields [10 ], k = mergeBoardFields [1]})
        (map getBoardFields [[6,13], [1,19],[1,14], [1,23], [1,28], [1,32]])

testJumpKingsPiecesList2 :: Test
testJumpKingsPiecesList2 = TestCase $ assertBool "test jump king pieces list 2"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [1, 6 ], wp = mergeBoardFields [10, 14 ], k = mergeBoardFields [1]})
        (map getBoardFields [[6,13]])


------------ BROKEN TEST ------------------
testJumpKingsPiecesList3 :: Test
testJumpKingsPiecesList3 = TestCase $ assertBool "test jump king pieces list 3"  $ assertBoolLists (getBlackJumpsList Board {bp = mergeBoardFields [1, 6 ], wp = mergeBoardFields [10, 23 ], k = mergeBoardFields [1]})
        (map getBoardFields [[6,13],[1,28], [1,32]])

getTestList :: [Test]
getTestList = [testMoveList1, testMoveList2, testMoveList3, testMoveList4, testMoveList5, testMoveList6,
         testJumpList1, testJumpList2, testJumpList3, testJumpList4, testJumpList5, testJumpList6,
         testJumpKingsList1, testJumpKingsList2, testJumpKingsList3, testJumpKingsList4, testJumpKingsList5, testJumpKingsList6,
         testJumpKingsPiecesList1, testJumpKingsPiecesList2]
