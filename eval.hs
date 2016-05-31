module Eval where

import Board
import Masks
import Data.Bits
import Data.Word
import Debug.Trace

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
maxEval = 40001

--- WEIGHTS


-- pieceWeight :: Float
-- pieceWeight = 5

-- kingWeight :: Float
-- kingWeight = 17.5


-- oppositeAreaWeight :: Float
-- oppositeAreaWeight = 2

-- protectFromKingLineWeight :: Float
-- protectFromKingLineWeight = 0.5

-- piecesOnEdgeWeight :: Float
-- piecesOnEdgeWeight = 2

--- END OF WEIGHTS

lazy = 64
maskEdges :: Word64
maskEdges = mergeBoardFields [25,21,17,13,9,1,8,12,16,20,24,28,32]

mask2Corner1 = mergeBoardFields [4, 8]
mask2Corner2 = mergeBoardFields [25, 29]
maskTop =  mergeBoardFields [21..32]
maskBottom =  mergeBoardFields [1..12]
mask7th = mergeBoardFields [25..28]

evaluate :: Board -> Player -> Int -> Int -> Int 
evaluate board player alpha beta
  | wp board == 0 = f $ -maxEval
  | bp board == 0 = f $ maxEval 
  | otherwise = f result
  where

    notKings = complement $ k board

    wpp = whitePieces board
    bpp = blackPieces board
    bk = blackKings board
    wk = whiteKings board
    nWK =  pieceCount wk
    nBK = pieceCount bk
    nWhite = pieceCount $ wp board
    nBlack = pieceCount $ bp board
    nPSqResult = nPSq (whiteKings board) (blackKings board)

    -- evaluate pieces, scaled higher for less material
    result1 :: Int
    result1 = if nWhite + nBlack > 12 then (nWhite - nBlack)*100 else (nWhite - nBlack)*(160 - (nWhite + nBlack)*5)
    result2 = result1 + nPSqResult

    -- winning advantage?
    result3 = if nWhite == 1 && nBlack >= 3 && nBlack < 8 then result2 + unsafeShiftR result2 1 else result2
    result4 = if nBlack == 1 && nWhite >= 3 && nWhite < 8 then result3 + unsafeShiftR result3 1 else result3

    --keep checkers off the edge
    result5 = result4 - 2*(pieceCount ((wp board .&. (complement $ k board)) .&. maskEdges)) - (pieceCount ((bp board .&. (complement $ k board)) .&. maskEdges))


    -- if losing move back and forth to edges
    result6 = if result5 > 18 && (bk .&. mask2Corner1) /= 0 then
                  if (mask2Corner1 .&. (complement $ bp board) .&. (complement $ wp board)) /= 0 then result5 - 18
                  else result5 - 8
              else result5
    result7 = if result6 > 18 && (bk .&. mask2Corner2) /= 0 then
                if (mask2Corner2 .&. (complement $ bp board) .&. (complement $ wp board)) /= 0 then result6 - 18
                else result6 - 8
              else result6

    result8 = if result7 > 18 && (wk .&. mask2Corner1) /= 0 then
            if (mask2Corner1 .&. (complement $ bp board) .&. (complement $ wp board)) /= 0 then result7 + 18
            else result7 + 8
        else result7
    result9 = if result8 > 18 && (wk .&. mask2Corner2) /= 0 then
                if (mask2Corner2 .&. (complement $ bp board) .&. (complement $ wp board)) /= 0 then result8 + 18
                else result8 + 8
              else result8

    -- endgame
    mul = if nWK == nBK && (nWhite+nBlack) < (nWK+nBK+2) then 8 else 2
    result10 = if nWK * 2 >= nWhite || nBK*2 >= nBlack || (nBK + nWK) >= 4 
               then result9 - mul * (pieceCount (wpp .&. maskBottom) - pieceCount(wpp .&. maskTop) - pieceCount(bpp .&. maskTop) + pieceCount(bpp .&. maskBottom))
               else result9

     --nBackB = (unsafeShiftR bpp 1) .&. 7
    --result11 = if nWK*2 < nWhite then result10 - backRowGuardB ()
    nAWK = nWK
    nABK = nBK

    -- active kings
    --result11 = if wk .&. maskTrappedW /= 0 then


    -- reward checkers what will king on next move
    --(bNearKing, result11) = if (bpp .&. mask7th) /= 0 && canBlackCheckerMove (bpp .&. mask7th) then (1, result10 - 33 + 5) else (0, result10)



    f = filterByPlayer player
    result = if result4 + lazy < alpha || result4 - lazy > beta then result4 else result10

nPSq :: Word64 -> Word64 -> Int
nPSq whiteKings blackKings
  | whiteKings /= 0 || blackKings /= 0 = res' + nPSq whiteKings' blackKings'
  | otherwise = 0
  where
    wIndex = rfield . s . getIndex $ whiteKings
    bIndex = rfield . s . getIndex $ blackKings
    whiteKings' = if whiteKings /= 0 then removePieceByIndex whiteKings (s . getIndex $ whiteKings) else 0
    blackKings' = if blackKings /= 0 then removePieceByIndex blackKings (s . getIndex $ blackKings) else 0
    res = if whiteKings /= 0 then kBoard wIndex else 0
    res' = if blackKings /= 0 then res + (negate . kBoard $ bIndex) else res

filterByPlayer :: Player -> Int -> Int
filterByPlayer p x = if p == White then x else -x


pieceCount :: (Word64 -> Int)
pieceCount = popCount