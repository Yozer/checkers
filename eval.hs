module Eval where


import Board
import Masks
import Data.Bits
import Data.Word


leftEdge = field_1 .|. field_9 .|. field_17 .|. field_25
rightEdge = field_8 .|. field_16 .|. field_24 .|.  field_32
bottomEdge = field_1 .|. field_2 .|. field_3 .|. field_4
topEdge = field_29 .|. field_30 .|. field_31 .|. field_32

bottomBoard = field_5 .|. field_6 .|. field_7 .|. 
         field_10 .|. field_11 .|. field_12 .|.
         field_13 .|. field_14 .|. field_15

topBoard = field_26 .|. field_27 .|. field_28 .|. 
         field_21 .|. field_22 .|. field_23 .|.
         field_18 .|. field_19 .|. field_20

pieceWeight :: Float
pieceWeight = 5
kingWeight :: Float
kingWeight = 17.5


oppositeAreaWeight = 2
protectFromKingLineWeight = 0.5
piecesOnEdgeWeight = 2

evaluate :: Board -> Player -> Float
evaluate board player =
  let
    resultType = ((kingWeight*) . pieceCount . whiteKings $ board) -
                 ((kingWeight*) . pieceCount . blackKings $ board) +
                 ((pieceWeight*) . pieceCount . whitePieces $ board) -
                 ((pieceWeight*) . pieceCount . blackPieces $ board)

    resultLocation = ((oppositeAreaWeight*) . pieceCount . wp $ board) + ((protectFromKingLineWeight*) . pieceCount . wp $ board) +  ((piecesOnEdgeWeight*) . pieceCount . wp $ board) -
                     ((oppositeAreaWeight*) . pieceCount . bp $ board) + ((protectFromKingLineWeight*) . pieceCount . bp $ board) +  ((piecesOnEdgeWeight*) . pieceCount . bp $ board)

    f = filterByPlayer player
    result = f resultType +f resultLocation 

  in result


filterByPlayer :: Player -> Float -> Float
filterByPlayer p x = if p == White then x else -x

pieceCount :: (Word64 -> Float)
pieceCount = fromIntegral . popCount