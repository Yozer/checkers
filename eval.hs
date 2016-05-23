module Eval where


import Board
import Masks
import Data.Bits
import Data.Word

leftEdge :: Word64
leftEdge = field_1 .|. field_9 .|. field_17 .|. field_25
rightEdge :: Word64
rightEdge = field_8 .|. field_16 .|. field_24 .|.  field_32
bottomEdge :: Word64
bottomEdge = field_1 .|. field_2 .|. field_3 .|. field_4
topEdge :: Word64
topEdge = field_29 .|. field_30 .|. field_31 .|. field_32
edges :: Word64
edges = leftEdge .|. rightEdge

bottomBoard :: Word64
bottomBoard = field_5 .|. field_6 .|. field_7 .|. 
         field_10 .|. field_11 .|. field_12 .|.
         field_13 .|. field_14 .|. field_15

topBoard :: Word64
topBoard = field_26 .|. field_27 .|. field_28 .|. 
         field_21 .|. field_22 .|. field_23 .|.
         field_18 .|. field_19 .|. field_20

pieceWeight :: Float
pieceWeight = 5
kingWeight :: Float
kingWeight = 17.5

oppositeAreaWeight :: Float
oppositeAreaWeight = 2

protectFromKingLineWeight :: Float
protectFromKingLineWeight = 0.5
piecesOnEdgeWeight :: Float
piecesOnEdgeWeight = 2

evaluate :: Board -> Player -> Float
evaluate board player = result
  where
    whites = wp board
    blacks = bp board

    resultType :: Float
    resultType = ((kingWeight*) . pieceCount . whiteKings $ board) -
                 ((kingWeight*) . pieceCount . blackKings $ board) +
                 ((pieceWeight*) . pieceCount . whitePieces $ board) -
                 ((pieceWeight*) . pieceCount . blackPieces $ board)

    resultLocation :: Float
    resultLocation = ((oppositeAreaWeight*) . pieceCount $ (whites .&. topBoard)) + ((protectFromKingLineWeight*) . pieceCount $ (whites .&. bottomEdge)) +  ((piecesOnEdgeWeight*) . pieceCount $ (whites .&. edges)) -
                     ((oppositeAreaWeight*) . pieceCount $ (blacks .&. bottomBoard)) + ((protectFromKingLineWeight*) . pieceCount $ (blacks .&. topBoard)) +  ((piecesOnEdgeWeight*) . pieceCount $  (blacks .&. edges))

    f = filterByPlayer player
    result = f resultType + f resultLocation 

filterByPlayer :: Player -> Float -> Float
filterByPlayer p x = if p == Black then x else -x

pieceCount :: (Word64 -> Float)
pieceCount = fromIntegral . popCount