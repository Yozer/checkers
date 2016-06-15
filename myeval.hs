{-# LANGUAGE ForeignFunctionInterface #-}
module Eval where

import           Board
import           Data.Bits
import           Data.Word
import           Masks
import Foreign
import Foreign.C

data GamePhase = Opening | MidGame | EndGame
-- color : 1- Black,  -1 = White

mate = 10000 :: Int
-- occupiet = 16 :: Int

-- f = 0 :: Int
white = 1 :: Int
black = 2 :: Int
-- man = 4 :: Int
-- king = 8 :: Int

evaluateR :: Board -> Player -> Int -> Int -> Int -> Int
evaluateR board player alpha beta depth
  | bp board == 0 = if player == Black then (depth - mate) else (mate - depth)
  | wp board == 0 = if player == White then (depth - mate) else (mate - depth)
  | phase == EndGame = evaluateEndGame

  where
    whitePieces = (complement $ k board) .&. wp board
    blackPieces = (complement $ k board) .&. bp board
    whiteKings = wp board .&. k board
    blackKings = bp board .&. k board
    notEmpty = wp board .|. bp board
    allPieces = whitePieces .|. blackPieces
    empty = (complement notEmpty) .&. bitsOnTheBoard
    nbm = fromIntegral . popCount $ blackPieces
    nbk = fromIntegral . popCount $ blackKings
    nwm = fromIntegral . popCount $ whitePieces
    nwk = fromIntegral . popCount $ whiteKings
    white = nwm + nwk
    black = nbm + nbk

    phase = getPhase (nbm + nbk + nwm + nwk)


    --END GAME
    evaluateEndGame
      | nbk > 0 && (white < (2 + nbk)) && (eval1 < 0) = 0
      | nwk > 0 && (black < (2 + nwk)) && (eval1 > 0) = 0
      | nbk == 1 && blackKingOnEdge && (not whiteKingOnEdge) && white <= 3 && (black <= 2 || eval3 < 500) = 0
      | nwk == 1 && whiteKingOnEdge && (not blackKingOnEdge) && black <= 3 && (white <= 2 || eval3 > -500) = 0

      where
        (whiteKingOnEdge, blackKingOnEdge) = kingOnLongEdge allPieces whiteKings blackKings
        eval1 = (100 * nbm + 300 * nbk) - (100 * nwm + 300 * nwk)
        eval2 = eval1 + if (white == 1 && nwm == 1 && black >= 4) || (black == 1 && nbm == 1 && white >= 4) then eval1 `div` 2 else 0
        eval3 = if (nbk > 0 && eval2 < 0) || (nwk > 0 && eval2 > 0) then eval2 `div` 2 else eval2
        eval4 = eval3 + (countPSTKings blackKings)
        eval5 = eval4 - (countPSTKings whiteKings)


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
  | x > 19 = Opening
  | x > 8 = MidGame
  | otherwise = EndGame

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