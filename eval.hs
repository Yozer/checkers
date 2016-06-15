{-# LANGUAGE ForeignFunctionInterface #-}
module Eval where

import           Board
import           Data.Bits
import           Data.Word
import           Masks
import Foreign
import Foreign.C
import Debug.Trace
import Control.Monad

mate = 10000 :: Int
occupiet = 16 :: Int

fe = 0 :: Int
white = 1 :: Int
black = 2 :: Int
man = 4 :: Int
king = 8 :: Int

evaluate :: Board -> Player -> Int -> Int -> Int -> IO Int
evaluate board player alpha beta depth = do
  (CInt result) <- c_eval (CULLong $ wp board) (CULLong $ bp board) (CULLong $ k board) color (getUint alpha) (getUint beta) (getUint depth)
  --result <- evaluateL board player alpha beta depth
  --when (result /= (fromIntegral result1)) $ trace ("diff: " ++ (show result) ++ ":" ++ (show result1) ++ ":  " ++ (show board)) $ error "wtf"
  return . fromIntegral $ result
  where
    color = getUint $ if player == White then white else black


getUint = CInt . fromIntegral


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

-- board -> color -> alpha -> beta -> blackPieces -> whitePieces -> blackKings -> whiteKings -> depth -> eval
foreign import ccall "evaluation" c_eval :: CULLong -> CULLong -> CULLong -> CInt -> CInt -> CInt -> CInt -> IO CInt
