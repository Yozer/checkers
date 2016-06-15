{-# LANGUAGE ForeignFunctionInterface #-}
module EvalLegacy where

import           Board
import           Data.Bits
import           Data.Word
import           Masks
import Foreign
import Debug.Trace
import Foreign.C

mateL = 10000 :: Int
occupiet = 16 :: Int

fe = 0 :: Int
white = 1 :: Int
black = 2 :: Int
man = 4 :: Int
king = 8 :: Int

evaluateL :: Board -> Player -> Int -> Int -> Int -> IO Int
evaluateL board player alpha beta depth = do
  b <- convertBoard board
  (CInt result) <- c_evalL b color (getUint alpha) (getUint beta) blackPiecesN whitePiecesN blackKingsN whiteKingsN (getUint depth)
  return . fromIntegral $ result
  where
    whitePiecesN = getUint $ popCount . whitePieces $ board
    blackPiecesN = getUint $ popCount . blackPieces $ board
    whiteKingsN = getUint $ popCount . whiteKings $ board
    blackKingsN = getUint $ popCount . blackKings $ board
    color = getUint $ if player == White then white else black


getUint = CInt . fromIntegral

-- mapper  = [-1,-1,-1, -1,-1,
--                      32,31,30,29, -1,
--                      28,27,26,25,
--                      24,23,22,21, -1,
--                      20,19,18,17,
--                      16, 15, 14, 13, -1,
--                      12, 11, 10, 9, 
--                      8,7,6,5, -1,
--                      4,3,2,1
--                      ] 

convertBoard :: Board -> IO (Ptr CInt)
convertBoard board = newArray board'
  where
    board' = map (\x -> getUint $ mapPiece x board) . map f $ [0..40]

mapPiece :: Int -> Board -> Int
mapPiece index board
  | index == -1 = occupiet
  | isKing && isWhite = white .|. king
  | isKing && isBlack = black .|. king
  | isWhite = white .|. man
  | isBlack = black .|. man
  | otherwise = fe
  where
    x = field . fromIntegral $ index
    isKing = k board .&. x /= 0
    isWhite = wp board .&. x /= 0
    isBlack = bp board .&. x /= 0

-- board -> color -> alpha -> beta -> blackPieces -> whitePieces -> blackKings -> whiteKings -> depth -> eval
foreign import ccall "evaluationL" c_evalL :: Ptr CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt