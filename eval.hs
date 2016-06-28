module Eval where

import           Board
import           Data.Bits
import qualified Data.Vector.Unboxed as V
import           Data.Word
import           Masks

opening = 1
midGame = 2
endGame = 3

mate = 10000 :: Int

-- najładniejszy kod...
-- pisany na baaardzo szybko


evaluate :: Board -> Player -> Int -> Int -> Int -> Int
evaluate board player alpha beta depth
  | bp board == 0 = if player == Black then (depth - mate) else (mate - depth)
  | wp board == 0 = if player == White then (depth - mate) else (mate - depth)
  | phase == endGame = evaluateEndGame
  | otherwise = evaluateGame
  where
    whitePieces = (complement $ k board) .&. (wp board)
    blackPieces = (complement $ k board) .&. (bp board)
    whiteKings = (wp board) .&. k board
    blackKings = (bp board) .&. k board
    notEmpty = (wp board) .|. (bp board)
    allPieces = whitePieces .|. blackPieces
    empty = (complement notEmpty) .&. bitsOnTheBoard
    nbm = popCount $ blackPieces
    nbk = popCount $ blackKings
    nwm = popCount $ whitePieces
    nwk = popCount $ whiteKings
    whiteN = nwm + nwk
    blackN = nbm + nbk

    phase = getPhase (nbm + nbk + nwm + nwk)
    color = if player == Black then 1 else -1


    --END GAME
    evaluateEndGame
      | nbk > 0 && (whiteN < (2 + nbk)) && (eval1 < 0) = 0
      | nwk > 0 && (blackN < (2 + nwk)) && (eval1 > 0) = 0
      | nbk == 1 && blackKingOnEdge && (not whiteKingOnEdge) && whiteN <= 3 && (blackN <= 2 || eval3 < 500) = 0
      | nwk == 1 && whiteKingOnEdge && (not blackKingOnEdge) && blackN <= 3 && (whiteN <= 2 || eval3 > -500) = 0
      | player == Black = eval10
      | otherwise = negate eval10
      where
        (whiteKingOnEdge, blackKingOnEdge) = kingOnLongEdge allPieces whiteKings blackKings
        (b_lattice, w_lattice) = calcLattice whitePieces blackPieces

        eval1 = (100 * nbm + 300 * nbk) - (100 * nwm + 300 * nwk)
        eval2 = eval1 + if (whiteN == 1 && nwm == 1 && blackN >= 4) || (blackN == 1 && nbm == 1 && whiteN >= 4) then eval1 `div` 2 else 0
        eval3 = if (nbk > 0 && eval2 < 0) || (nwk > 0 && eval2 > 0) then unsafeShiftR eval2 1 else eval2
        eval4 = eval3 + (countPSTKings blackKings)
        eval5 = eval4 - (countPSTKings whiteKings)
        eval6 = eval5 + (countValuePieces whitePieces blackPieces)
        eval7 = eval6 + if w_lattice /= 0 then w_lattice - 2 else 0
        eval8 = eval7 - if b_lattice /= 0 then b_lattice - 2 else 0
        eval9 = eval8 + calcStones
        eval10 = eval9 + if player == Black then 1 else -1


        calcStones
          | nbk /= 0 || nwk /= 0 || nbm /= nwm = 0
          | player == Black = resultBlack
          | otherwise = resultWhite
          where
            allStones = nbm + nwm
            stonestInSystemBlack = popCount (verticalTopEdges .&. notEmpty)
            resultBlack = if (stonestInSystemBlack `mod` 2) /= 0 then (2*(24-allStones)) `div` 6 else -(2*(24-allStones)) `div` 6
            stonestInSystemWhite = popCount (verticalNoTopEdges .&. notEmpty)
            resultWhite = if (stonestInSystemWhite `mod` 2) /= 0 then -(2*(24-allStones)) `div` 6 else (2*(24-allStones)) `div` 6

    evaluateGame
      | (lazyEval - 64 >= beta) || (lazyEval + 64 <= alpha) = lazyEval
      | player == Black = eval35
      | otherwise = negate eval35
      where
        balanceKings
          | nbk == nwk = 0
          | otherwise = res
          where
            res
              | nwk == 0 && nbm >= nwm - 2 = 200
              | nbk == 0 && nwm >= nbm - 2 = -200
              | otherwise = 0

        scaleDown
          | (nbk > 0 && eval2 < 0) || (nwk > 0 && eval2 > 0) = (unsafeShiftR (3 * eval2) 2)
          | otherwise = eval2
        checkSingleCorner = res1 + res2
          where
            res1
              | (empty .&. mergeBoardFields [32,28]) == mergeBoardFields [32,28] = if empty .&. field 31 /= 0 then 3 else 8
              | otherwise = 0
            res2
              | (empty .&. mergeBoardFields [1,5]) == mergeBoardFields [1,5] = if empty .&. field 2 /= 0 then -3 else -8
              | otherwise = 0
        v1 = 100 * nbm + 250 * nbk
        v2 = 100 * nwm + 250 * nwk
        a :: Double
        a = fromIntegral $ (200 * (v1 - v2))
        b :: Double
        b = fromIntegral $ (v1 + v2)
        eval1 = (v1 - v2) + (truncate  (a / b))
        eval2 = eval1 + balanceKings
        eval3 = scaleDown
        lazyEval = if player == White then negate eval3 else eval3
        eval4 = eval3 + calculateBackrank allPieces whitePieces blackPieces phase
        eval5 = eval4 + balance whitePieces blackPieces whiteKings blackKings nbm nwm
        eval6 = eval5 + checkSingleCorner
        eval7 = centerControl eval6 (bp board) empty (field 18) (field 22) (field 27) (field 21) (field 25) color
        eval8 = centerControl eval7 (bp board) empty (field 19) (field 23) (field 28) (field 22) (field 26) color
        eval9 = centerControl eval8 (bp board) empty (field 22) (field 27) (field 31) (field 26) (field 29) color

        condition1 = ((bp board) .&. field 15) /= 0 && (empty .&. (mergeBoardFields [12,11,2])) == (mergeBoardFields [12,11,2])
        eval10 = if condition1 then isGoodPiece (eval9 + 2) (bp board) empty (field 20) (field 24) color else eval9
        eval11 = if condition1 then isGoodPiece eval10 (bp board) empty (field 19) (field 22) color else eval10

        condition2 = ((bp board) .&. field 11) /= 0 && (empty .&. field 3) /= 0
        eval12 = if condition2 then isGoodPiece (eval11 + 2) (bp board) empty (field 15) (field 20) color else eval11
        eval13 = if condition2 then isGoodPiece eval12 (bp board) empty (field 14) (field 18) color else eval12

        condition3 = ((bp board) .&. field 10) /= 0 && (empty .&. field 2) /= 0
        eval14 = if condition3 then isGoodPiece (eval13 + 2) (bp board) empty (field 14) (field 19) color else eval13
        eval15 = if condition3 then isGoodPiece eval14 (bp board) empty (field 13) (field 17) color else eval14

        eval16 = centerControl eval15 (wp board) empty (field 15) (field 12) (field 8) (field 11) (field 6) color
        eval17 = centerControl eval16 (wp board) empty (field 11) (field 7) (field 4) (field 6) (field 2) color
        eval18 = centerControl eval17 (wp board) empty (field 14) (field 11) (field 7) (field 10) (field 5) color

        condition4 = ((wp board) .&. field 18) /= 0 && (empty .&. (mergeBoardFields [22,21,31])) == (mergeBoardFields [22,21,31])
        eval19 = if condition4 then isGoodPiece (eval18 - 2) (wp board) empty (field 14) (field 11) color else eval18
        eval20 = if condition4 then isGoodPiece eval19 (wp board) empty (field 13) (field 9) color else eval19

        condition5 = ((wp board) .&. field 22) /= 0 && (empty .&. field 30) /= 0
        eval21 = if condition5 then isGoodPiece (eval20 - 2) (wp board) empty (field 19) (field 15) color else eval20
        eval22 = if condition5 then isGoodPiece eval21 (wp board) empty (field 18) (field 13) color else eval21

        condition6 = ((wp board) .&. field 23) /= 0 && (empty .&. field 31) /= 0
        eval23 = if condition6 then isGoodPiece (eval22 - 2) (wp board) empty (field 20) (field 16) color else eval22
        eval24 = if condition6 then isGoodPiece eval23 (wp board) empty (field 19) (field 14) color else eval23

        eval25 = eval24 - (popCount ((bp board) .&. (mergeBoardFields [25,17,24])))
        eval26 = eval25 + (popCount ((bp board) .&. (mergeBoardFields [16,8])))
        eval27 = eval26 + if ((bp board) .&. field 9 /= 0) .&. (empty .&. field 2 /= 0) then 1 else 0
        eval28 = eval27 + (popCount ((wp board) .&. (mergeBoardFields [8,16,9])))
        eval29 = eval28 - (popCount ((wp board) .&. (mergeBoardFields [17,25])))
        eval30 = eval29 + if ((wp board) .&. field 24 /= 0) .&. (empty .&. field 31 /= 0) then -1 else 0

        extraEval1
          | (notEmpty .&. field 14 /= 0) && (bp board .&. field 14) /= 0 = res
          | otherwise = 0
          where
            f = e2
              where
                e1 = isGoodPiece 1 (bp board) empty (field 19) (field 23) 1
                e2 = isGoodPiece e1 (bp board) empty (field 18) (field 21) 1
            res
              | phase /= opening = f
              | otherwise = -4

        extraEval2
          | (notEmpty .&. field 19 /= 0) && (wp board .&. field 19) /= 0 = res
          | otherwise = 0
          where
            f = e2
              where
                e1 = isGoodPiece (-1) (wp board) empty (field 15) (field 12) (-1)
                e2 = isGoodPiece e1 (wp board) empty (field 14) (field 10) (-1)
            res
              | phase /= opening = f
              | otherwise = 4

        bonusForKingFactor = if phase == opening then 8 else 16
        bonusForKingFactorBlack = if player == Black then unsafeShiftL bonusForKingFactor 1 else bonusForKingFactor
        bonusForKingFactorWhite = if player == White then unsafeShiftL bonusForKingFactor 1 else bonusForKingFactor
        eval31 = extraEval1 + extraEval2 + eval30 + bonusForKingFactorBlack*popCount (((unsafeShiftL (row1 .&. empty) 1) .&. blackPieces) .|. ( (unsafeShiftL (row1 .&. empty) 9) .&. blackPieces))
        eval32 = eval31 - bonusForKingFactorWhite*popCount (((unsafeShiftR (row8 .&. empty) 1) .&. whitePieces) .|. ( (unsafeShiftR (row8 .&. empty) 9) .&. whitePieces))

        w_lattice = abs $ 4*popCount (lattice .&. whitePieces) - 4*popCount(latticeMinus .&. whitePieces)
        b_lattice = abs $ 4*popCount (lattice .&. blackPieces) - 4*popCount(latticeMinus .&. blackPieces)
        eval33 = eval32 + if w_lattice > 0 then w_lattice - 2 else 0
        eval34 = eval33 - if b_lattice > 0 then b_lattice - 2 else 0

        eval35 = eval34 + if phase == opening then evalOpeningPhase whitePieces blackPieces else 0

evalOpeningPhase whitePieces blackPieces =  res6
  where
    res1 = if blackPieces .&. field 23 /= 0 then -1 else 0
    res2 = res1 + if blackPieces .&. field 22 /= 0 then -1 else 0
    res3 = res2 + if blackPieces .&. field 29 /= 0 then 5 else 0
    res4 = res3 + if whitePieces .&. field 10 /= 0 then 1 else 0
    res5 = res4 + if whitePieces .&. field 11 /= 0 then 1 else 0
    res6 = res5 + if whitePieces .&. field 4 /= 0 then -5 else 0

balance whitePieces blackPieces whiteKings blackKings nbm nwm
  | nbm == nwm = eval1 + eval2 + eval3 + eval4 + eval5 + eval6
  | otherwise = 0
  where
    leftFlank = mergeBoardFields [32,24,16,8,28,20,12,4,31,23,15,7]
    rightFlank = mergeBoardFields [26,18,10,2,29,21,13,5,25,17,9,1]
    code = popCount (whitePieces .&. leftFlank) +
          256*popCount (blackPieces .&. leftFlank) +
          16*popCount (whiteKings .&. leftFlank) +
          4096*popCount (blackKings .&. leftFlank)
    nwml = code .&. 15
    nbml = (unsafeShiftR code 8) .&. 15

    code1 = popCount (whitePieces .&. rightFlank) +
         256*popCount (blackPieces .&. rightFlank) +
         16*popCount (whiteKings .&. rightFlank) +
         4096*popCount (blackKings .&. rightFlank)
    nwmr = code1 .&. 15
    nbmr = (unsafeShiftR code1 8) .&. 15
    balanc = abs (nbml - nbmr)
    balanc1 = abs (nwml - nwmr)

    eval' = if nwml == 0 then 10 else 0
    eval0 = eval' + if nbml == 0 then -10 else 0
    eval1 = eval0 + if nwmr == 0 then 10 else 0
    eval2 = if nbmr == 0 then -10 else 0
    eval3 = if balanc >= 2 then negate $ unsafeShiftL balanc 1 else 0
    eval4 = if balanc1 >= 2 then unsafeShiftL balanc1 1 else 0
    eval5 = if nbml + nbmr == nbm then -4 else 0
    eval6 = if nwml + nwmr == nwm then 4 else 0

calculateBackrank allPieces whitePieces blackPieces phase = brv*backRank2
  where
    brv = if phase == opening then 3 else 1
    code1 = if (allPieces .&. (field 32) /= 0) then 1 else 0
    code2 = code1 + if (allPieces .&. (field 31) /= 0) then 2 else 0
    code3 = code2 + if (allPieces .&. (field 30) /= 0) then 4 else 0
    code4 = code3 + if (allPieces .&. (field 29) /= 0) then 8 else 0
    code5 = code4 + if (blackPieces .&. (field 25) /= 0) then 16 else 0
    backRank1 = brTable code5

    code6 = if (allPieces .&. (field 4) /= 0) then 8 else 0
    code7 = code6 + if (allPieces .&. (field 3) /= 0) then 4 else 0
    code8 = code7 + if (allPieces .&. (field 2) /= 0) then 2 else 0
    code9 = code8 + if (allPieces .&. (field 1) /= 0) then 1 else 0
    code10 = code9 + if (whitePieces .&. (field 8) /= 0) then 16 else 0
    backRank2 = backRank1 - (brTable code10)

brTable i = (V.fromList [0,-1,1,0,3,3,3,3,2,2,2,2,4,4,8,7,1,0,1,0,3,3,3,3,2,2,2,2,4,4,8,7]) V.! i

calcLattice whitePieces blackPieces = (abs $ res1 + res2, abs $ res3 + res4)
  where
    res1 = 2*popCount (lattice .&. blackPieces)
    res2 = -2*popCount (latticeMinus .&. blackPieces)
    res3 = 2*popCount (lattice .&. whitePieces)
    res4 = -2*popCount (latticeMinus .&. whitePieces)

countValuePieces whitePieces blackPieces = res1 + res2 + res3 + res4 + res5 + res6
                                           -res7 - res8 - res9 - res10 - res11 - res12
  where
    res1 = 2*popCount (row7 .&. blackPieces)
    res2 = 4*popCount ((row6 .|. (field 9) .|. (field 8)) .&. blackPieces)
    res3 = 6*popCount (row5 .&. blackPieces)
    res4 = 8*popCount (row4 .&. blackPieces)
    res5 = 10*popCount ((row3 `xor` (field 9)) .&. blackPieces)
    res6 = 12*popCount ((row2 `xor` (field 8)) .&. blackPieces)

    res7 = 12*popCount ((row7 `xor` (field 25)) .&. whitePieces)
    res8 = 10*popCount ((row6 `xor` (field 24)) .&. whitePieces)
    res9 = 8*popCount (row5 .&. whitePieces)
    res10 = 6*popCount (row4 .&. whitePieces)
    res11 = 4*popCount ((row3 .|. (field 25) .|. (field 24)) .&. whitePieces)
    res12 = 2*popCount (row2 .&. whitePieces)

countPSTKings kings = pst1 + pst2 + pst3
  where
    pst1 = popCount (pstKing1 .&. kings)
    pst2 = 2* popCount (pstKing2 .&. kings)
    pst3 = 3* popCount (pstKing3 .&. kings)

kingOnLongEdge allPieces whiteKings blackKings
  | pieceOnEdge = (False, False)
  | otherwise = (whiteKingOnEdge, blackKingOnEdge)
  where
    pieceOnEdge = (allPieces .&. longEdge) /= 0
    whiteKingOnEdge = (whiteKings .&. longEdge) /= 0
    blackKingOnEdge = (blackKings .&. longEdge) /= 0

getPhase x
  | x > 19 = opening
  | x > 8 = midGame
  | otherwise = endGame

isGoodPiece eval pieces empty sq1 sq2 color
  | pieces .&. sq1 /= 0 = f
  | otherwise = eval
  where
    f
      | pieces .&. sq2 /= 0 = eval + color
      | empty .&. sq2 /= 0 = eval - color
      | otherwise = eval


centerControl eval pieces empty sq1 sq2 sq3 sq4 sq5 color
  | pieces .&. sq1 /= 0 = result
  | otherwise = eval
  where
    eval' = isGoodPiece (eval + color) pieces empty sq2 sq3 color
    result = isGoodPiece eval' pieces empty sq4 sq5 color


pstKing1 = mergeBoardFields [31,27,24,20,13,9,6,2]
pstKing2 = mergeBoardFields [32,29,28,26,25,23,21,12,10,8,7,5,4,1]
pstKing3 = mergeBoardFields [22,19,18,11,15,14]
longEdge = mergeBoardFields [32,28,23,19,14,10,5,1]
row1 = mergeBoardFields [1..4]
row2 = mergeBoardFields [5..8]
row3 = mergeBoardFields [9..12]
row4 = mergeBoardFields [13..16]
row5 = mergeBoardFields [17..20]
row6 = mergeBoardFields [21..24]
row7 = mergeBoardFields [25..28]
row8 = mergeBoardFields [29..32]
lattice = 18102720552763392 :: Word64
latticeMinus = 35356876079616 :: Word64
verticalTopEdges = 9268593481931686400 :: Word64
verticalNoTopEdges = 18102721644397825 :: Word64
