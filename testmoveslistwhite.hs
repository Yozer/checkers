module TestMovesListWhite where

import           Board
import           Data.Bits
import           Masks
import           Moves
import           Test.HUnit

testMoveList1 :: Test
testMoveList1 = TestCase $ assertEqual "test move list 1" (getWhiteMovesList initialBoard)
    MoveData
    {moves = [Move {src = field_12, dst = field_16},Move {src = field_12, dst = field_15},Move {src = field_11, dst = field_14},
    Move {src = field_11, dst = field_15},Move {src = field_10, dst = field_13},Move {src = field_10, dst = field_14},Move {src = field_9, dst = field_13}], movesCount = 7}

testMoveList2 :: Test
testMoveList2 = TestCase $ assertEqual "test move list 2" (getWhiteMovesList Board {wp = field_1, bp = 0, k = field_1})
    MoveData
    {moves = [Move {src = field_1, dst = field_5},Move {src = field_1, dst = field_10},Move {src = field_1, dst = field_14},
    Move {src = field_1, dst = field_19},Move {src = field_1, dst = field_23},Move {src = field_1, dst = field_28},Move {src = field_1, dst = field_32}], movesCount = 7}

testMoveList3 :: Test
testMoveList3 = TestCase $ assertEqual "test move list 3" (getWhiteMovesList Board {wp = field_14, bp = 0, k = field_14})
    MoveData
    {moves = [Move {src = field_14, dst = field_5},Move {src = field_14, dst = field_10},Move {src = field_14, dst = field_1},
    Move {src = field_14, dst = field_19},Move {src = field_14, dst = field_23},Move {src = field_14, dst = field_28},Move {src = field_14, dst = field_32},
    Move {src = field_14, dst = field_18},Move {src = field_14, dst = field_21},Move {src = field_14, dst = field_25},
    Move {src = field_14, dst = field_11}, Move {src = field_14, dst = field_7},Move {src = field_14, dst = field_4}], movesCount = 13}

testMoveList4 :: Test
testMoveList4 = TestCase $ assertEqual "test move list 4" (getWhiteMovesList Board {wp = field_9 .|. field_21 .|. field_7, bp = 0, k = field_21})
    MoveData
    {moves = [Move {src = field_9, dst = field_13},Move {src = field_21, dst = field_25},Move {src = field_21, dst = field_18},
    Move {src = field_21, dst = field_14},Move {src = field_21, dst = field_11},Move {src = field_7, dst = field_11},Move {src = field_7, dst = field_12},
    Move {src = field_21, dst = field_17},Move {src = field_21, dst = field_26},Move {src = field_21, dst = field_30}], movesCount = 10}

testMoveList5 :: Test
testMoveList5 = TestCase $ assertEqual "test move list 5" (getWhiteMovesList Board {wp = field_27 .|. field_20 .|. field_15, bp = field_30, k = field_20})
    MoveData
    {moves = [Move {src = field_27, dst = field_31},Move {src = field_15, dst = field_19},Move {src = field_20, dst = field_23},
    Move {src = field_20, dst = field_24},Move {src = field_20, dst = field_16}], movesCount = 5}

testMoveList6 :: Test
testMoveList6 = TestCase $ assertEqual "test move list 6" (getWhiteMovesList Board {wp = field_18 .|. field_11 .|. field_14, bp = field_25 .|. field_21 .|. field_22 .|. field_27, k = field_14})
    MoveData
    {moves = [Move {src = field_11, dst = field_15},Move {src = field_14, dst = field_1},Move {src = field_14, dst = field_5},
    Move {src = field_14, dst = field_10},Move {src = field_14, dst = field_19},Move {src = field_14, dst = field_23},Move {src = field_14, dst = field_28},Move {src = field_14, dst = field_32}], movesCount = 8}



-------- PIECES JUMPS ---------------
testJumpList1 :: Test
testJumpList1 = TestCase $ assertEqual "test jump list 1" (getWhiteJumpsList Board {wp = field_14, bp = field_18 .|. field_19 .|. field_10 .|. field_11, k = 0})
        JumpData {jumps = [Jump {jumpPath = [field_14, field_21], jumpSize = 1},Jump {jumpPath = [field_14, field_23], jumpSize = 1},
                   Jump {jumpPath = [field_14, field_5], jumpSize = 1}, Jump {jumpPath = [field_14, field_7], jumpSize = 1}],  jumpsCount = 4}

testJumpList2 :: Test
testJumpList2 = TestCase $ assertEqual "test jump list 2" (getWhiteJumpsList Board {wp = field_5, bp = field_10, k = 0})
        JumpData {jumps = [Jump {jumpPath = [field_5, field_14], jumpSize = 1}],  jumpsCount = 1}

testJumpList3 :: Test
testJumpList3 = TestCase $ assertEqual "test jump list 3" (getWhiteJumpsList Board {wp = field_14, bp = field_18 .|. field_19 .|. field_10 .|. field_11 .|. field_28, k = 0})
        JumpData {jumps = [Jump {jumpPath = [field_14, field_21], jumpSize = 1},Jump {jumpPath = [field_14, field_23, field_32], jumpSize = 2},
                   Jump {jumpPath = [field_14, field_5], jumpSize = 1}, Jump {jumpPath = [field_14, field_7], jumpSize = 1}],  jumpsCount = 4}

testJumpList4 :: Test
testJumpList4 = TestCase $ assertEqual "test jump list 4" (getWhiteJumpsList Board {wp = field_12, bp = field_15  .|. field_23 .|. field_22 .|. field_21  .|. field_13 .|. field_6 .|. field_14, k = 0})
        JumpData {jumps = [Jump {jumpPath = [field_12, field_19, field_28], jumpSize = 2}, Jump {jumpPath = [field_12, field_19, field_26, field_17, field_10, field_3], jumpSize = 5},
        Jump {jumpPath = [field_12, field_19, field_26, field_17, field_10, field_19, field_28], jumpSize = 6}, Jump {jumpPath = [field_12, field_19, field_10, field_3], jumpSize = 3},
        Jump {jumpPath = [field_12, field_19, field_10, field_17, field_26, field_19, field_28], jumpSize = 6}],jumpsCount = 5}


testJumpList5 :: Test
testJumpList5 = TestCase $ assertEqual "test jump list 5" (getWhiteJumpsList Board {wp = field_14, bp = field_19  .|. field_28, k = 0})
        JumpData {jumps = [Jump {jumpPath = [field_14,field_23,field_32], jumpSize = 2}], jumpsCount = 1}

testJumpList6 :: Test
testJumpList6 = TestCase $ assertEqual "test jump list 6" (getWhiteJumpsList Board {wp = field_19 .|. field_28, bp = field_14  .|. field_23, k = 0})
        JumpData {jumps = [Jump {jumpPath = [field_19,field_10], jumpSize = 1}], jumpsCount = 1}

-------------- KINGS JUMPS -------------
testJumpKingsList1 :: Test
testJumpKingsList1 = TestCase $ assertEqual "test jump king list 1" (getWhiteJumpsList Board {wp = field_10, bp = field_14, k = field_10})
           JumpData {jumps = [Jump {jumpPath = [field_10,field_19], jumpSize = 1}, Jump {jumpPath = [field_10,field_23], jumpSize = 1},
                    Jump {jumpPath = [field_10,field_28], jumpSize = 1}, Jump {jumpPath = [field_10,field_32], jumpSize = 1}],
           jumpsCount = 4}

testJumpKingsList2 :: Test
testJumpKingsList2 = TestCase $ assertEqual "test jump king list 2" (getWhiteJumpsList Board {wp = field_10, bp = field_28, k = field_10})
           JumpData {jumps = [Jump {jumpPath = [field_10,field_32], jumpSize = 1}],
           jumpsCount = 1}

testJumpKingsList3 :: Test
testJumpKingsList3 = TestCase $ assertEqual "test jump king list 3" (getWhiteJumpsList Board {wp = field_14, bp = field_18 .|. field_19 .|. field_10 .|. field_11, k = field_14})
        JumpData {jumps = [Jump {jumpPath = [field_14, field_21], jumpSize = 1},Jump {jumpPath = [field_14, field_25], jumpSize = 1},Jump {jumpPath = [field_14, field_23], jumpSize = 1},
                   Jump {jumpPath = [field_14, field_28], jumpSize = 1}, Jump {jumpPath = [field_14, field_32], jumpSize = 1},
                   Jump {jumpPath = [field_14, field_5], jumpSize = 1}, Jump {jumpPath = [field_14, field_1], jumpSize = 1},
                   Jump {jumpPath = [field_14, field_7], jumpSize = 1}, Jump {jumpPath = [field_14, field_4], jumpSize = 1}],  jumpsCount = 9}

testJumpKingsList4 :: Test
testJumpKingsList4 = TestCase $ assertEqual "test jump king list 4" (getWhiteJumpsList Board {wp = field_14, bp = field_18 .|. field_19 .|. field_10 .|. field_11 .|. field_28, k = field_14})
        JumpData {jumps = [Jump {jumpPath = [field_14, field_21], jumpSize = 1},Jump {jumpPath = [field_14, field_25], jumpSize = 1},Jump {jumpPath = [field_14, field_23,field_32], jumpSize = 2},
                   Jump {jumpPath = [field_14, field_5], jumpSize = 1}, Jump {jumpPath = [field_14, field_1], jumpSize = 1},
                   Jump {jumpPath = [field_14, field_7], jumpSize = 1}, Jump {jumpPath = [field_14, field_4], jumpSize = 1}],  jumpsCount = 7}

testJumpKingsList5 :: Test
testJumpKingsList5 = TestCase $ assertEqual "test jump king list 5" (getWhiteJumpsList Board {wp = field_12, bp = field_15  .|. field_23 .|. field_22 .|. field_21  .|. field_13 .|. field_6 .|. field_14, k = field_12})
        JumpData {jumps = [Jump {jumpPath = [field_12, field_19, field_28], jumpSize = 2}, Jump {jumpPath = [field_12, field_19, field_32], jumpSize = 2}, 
        Jump {jumpPath = [field_12, field_19, field_26, field_17, field_10, field_3], jumpSize = 5},
        Jump {jumpPath = [field_12, field_19, field_26, field_17, field_10, field_19, field_28], jumpSize = 6}, Jump {jumpPath = [field_12, field_19, field_26, field_17, field_10, field_19, field_32], jumpSize = 6}, 
        Jump {jumpPath = [field_12, field_19, field_10, field_3], jumpSize = 3},
        Jump {jumpPath = [field_12, field_19, field_10, field_17, field_26, field_19, field_28], jumpSize = 6}, 
        Jump {jumpPath = [field_12, field_19, field_10, field_17, field_30, field_20], jumpSize = 5},
        Jump {jumpPath = [field_12, field_19, field_10, field_17, field_30, field_16], jumpSize = 5},
        Jump {jumpPath = [field_12, field_19, field_10, field_17, field_26, field_19, field_32], jumpSize = 6}],jumpsCount = 10}


testJumpKingsList6 :: Test
testJumpKingsList6 = TestCase $ assertEqual "test jump king list 6" (getWhiteJumpsList Board {wp = field_14, bp = field_19  .|. field_28, k = field_14})
        JumpData {jumps = [Jump {jumpPath = [field_14,field_23,field_32], jumpSize = 2}], jumpsCount = 1}   


------- KINGS AND PIECES JUMPS -------------

testJumpKingsPiecesList1 :: Test
testJumpKingsPiecesList1 = TestCase $ assertEqual "test jump king pieces list 1" (getWhiteJumpsList Board {wp = field_1 .|. field_6 , bp = field_10 , k = field_1})
        JumpData {jumps = [Jump {jumpPath = [field_6,field_13], jumpSize = 1}, Jump {jumpPath = [field_1,field_19], jumpSize = 1},
                           Jump {jumpPath = [field_1,field_14], jumpSize = 1}, Jump {jumpPath = [field_1,field_23], jumpSize = 1}, 
                           Jump {jumpPath = [field_1,field_28], jumpSize = 1}, Jump {jumpPath = [field_1,field_32], jumpSize = 1}], jumpsCount = 6}  

testJumpKingsPiecesList2 :: Test
testJumpKingsPiecesList2 = TestCase $ assertEqual "test jump king pieces list 2" (getWhiteJumpsList Board {wp = field_1 .|. field_6 , bp = field_10 .|. field_14 , k = field_1})
        JumpData {jumps = [Jump {jumpPath = [field_6,field_13], jumpSize = 1}], jumpsCount = 1}  


------------ BROKEN TEST ------------------
testJumpKingsPiecesList3 :: Test
testJumpKingsPiecesList3 = TestCase $ assertEqual "test jump king pieces list 3" (getWhiteJumpsList Board {wp = field_1 .|. field_6 , bp = field_10 .|. field_23 , k = field_1})
        JumpData {jumps = [Jump {jumpPath = [field_6,field_13], jumpSize = 1},
                           Jump {jumpPath = [field_1,field_28], jumpSize = 1}, Jump {jumpPath = [field_1,field_32], jumpSize = 1}], jumpsCount = 3}  

getTestList = [testMoveList1, testMoveList2, testMoveList3, testMoveList4, testMoveList5, testMoveList6,
         testJumpList1, testJumpList2, testJumpList3, testJumpList4, testJumpList5, testJumpList6,
         testJumpKingsList1, testJumpKingsList2, testJumpKingsList3, testJumpKingsList4, testJumpKingsList5, testJumpKingsList6,
         testJumpKingsPiecesList1, testJumpKingsPiecesList2]
