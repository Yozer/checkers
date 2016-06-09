module Eval where

import           Board
import           Data.Bits
import           Data.Word
import           Masks

leftEdge :: Word64
leftEdge = mergeBoardFields [1,9,17,25]
rightEdge :: Word64
rightEdge = mergeBoardFields [8,16,24,32]
bottomEdge :: Word64
bottomEdge = mergeBoardFields [1..4]
topEdge :: Word64
topEdge =mergeBoardFields [29..32]
edges :: Word64
edges = leftEdge .|. rightEdge

bottomBoard :: Word64
bottomBoard = mergeBoardFields [5,6,7,10,11,12,13,14,15]

topBoard :: Word64
topBoard = mergeBoardFields [26,27,28,21,22,23,18,19,20]

maxEval :: Int
maxEval = 400001

-- WEIGHTS

pieceWeight = 10
kingWeight = 35
oppositeAreaWeight = 4
protectFromKingLineWeight = 1
piecesOnEdgeWeight = 4


--- END OF WEIGHTS


evaluate :: Board -> Player -> Int
evaluate board player
  | wp board == 0 = f (result - 25000)
  | bp board == 0 = f (result + 25000)
  | otherwise = f result
  where
    whites = wp board
    blacks = bp board
    whitePieces = notKings .&. wp board
    blackPieces = notKings .&. bp board

    notKings = complement $ k board
    wKingsCount = pieceCount . whiteKings $ board
    bKingsCount = pieceCount . blackKings $ board
    wPiecesCount = pieceCount $ notKings .&. wp board
    bPiecesCount = pieceCount $ notKings .&. bp board
    resultType = kingWeight * (wKingsCount - bKingsCount) + pieceWeight * (wPiecesCount - bPiecesCount)

    resultLocation = ((oppositeAreaWeight*) . pieceCount $ (whitePieces .&. topBoard)) + ((protectFromKingLineWeight*) . pieceCount $ (whitePieces .&. bottomEdge)) +  ((piecesOnEdgeWeight*) . pieceCount $ (whites .&. edges)) -
                     ((oppositeAreaWeight*) . pieceCount $ (blackPieces .&. bottomBoard)) - ((protectFromKingLineWeight*) . pieceCount $ (blackPieces .&. topEdge)) -  ((piecesOnEdgeWeight*) . pieceCount $  (blacks .&. edges))


    f = filterByPlayer player
    result = resultLocation + resultType

filterByPlayer :: Player -> Int -> Int
filterByPlayer p x = if p == White then x else -x


pieceCount :: (Word64 -> Int)
pieceCount = popCount
