module Eval where


import Board
import Masks
import Data.Bits
import Data.Word

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
maxEval :: Float
maxEval = 401

--- WEIGHTS

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

--- END OF WEIGHTS

evaluate :: Board -> Player -> Float
evaluate board player = if player == White then  result else negate result
  where
    -- whites = wp board
    -- blacks = bp board

    notKings = complement $ k board
    wKingsCount = pieceCount . whiteKings $ board
    bKingsCount = pieceCount . blackKings $ board
    wPiecesCount = pieceCount $ notKings .&. wp board
    bPiecesCount = pieceCount $ notKings .&. bp board
    resultType = kingWeight * (wKingsCount - bKingsCount) + pieceWeight * (wPiecesCount - bPiecesCount)

    --resultLocation :: Float
    --resultLocation = ((oppositeAreaWeight*) . pieceCount $ (whites .&. topBoard)) + ((protectFromKingLineWeight*) . pieceCount $ (whites .&. bottomEdge)) +  ((piecesOnEdgeWeight*) . pieceCount $ (whites .&. edges)) -
                     --((oppositeAreaWeight*) . pieceCount $ (blacks .&. bottomBoard)) + ((protectFromKingLineWeight*) . pieceCount $ (blacks .&. topBoard)) +  ((piecesOnEdgeWeight*) . pieceCount $  (blacks .&. edges))

    f = filterByPlayer player
    result = resultType -- + f resultLocation 

filterByPlayer :: Player -> Float -> Float
filterByPlayer p x = if p == White then x else -x

pieceCount :: (Word64 -> Float)
pieceCount = fromIntegral . popCount