module Board where

import           Data.Bits
import           Data.List
import           Data.List.Split
import           Data.Word
import           Masks
import Data.Maybe
import Data.Char

data Figure = BlackPiece | WhitePiece | BlackKing | WhiteKing | Empty | Special deriving (Show, Eq)
data Player = White | Black deriving(Show, Eq)

data Board = Board {
  wp :: Word64,
  bp :: Word64,
  k  :: Word64
} deriving (Show, Eq)

data Move = Move {
  src :: Word64,
  dst :: Word64
} deriving(Show, Eq, Ord)

type MoveList = [Move]
type JumpList = [Jump]

data MoveData = MoveData {
  moves :: MoveList,
  movesCount :: Int
} deriving(Show)

data Jump = Jump {
  jumpPath :: [Word64],
  jumpSize :: Int
} deriving(Show, Eq, Ord)

data JumpData = JumpData {
  jumps :: [Jump],
  jumpsCount :: Int
} deriving(Show)

instance Eq MoveData where
    a == b = (movesCount a) == (movesCount b) && (sort $ moves a) == (sort $ moves b)

instance Eq JumpData where
  a == b = (jumpsCount a) == (jumpsCount b) && (sort $ jumps a) == (sort $ jumps b)

empty :: Board -> Word64
empty b = (complement (bp b .|. wp b)) .&. bitsOnTheBoard

piecesOnly :: Board -> Word64
piecesOnly b = (complement $ k b) .&. bitsOnTheBoard

whiteKings :: Board -> Word64
whiteKings b = k b .&. wp b

blackKings :: Board -> Word64
blackKings b = k b .&. bp b

isKing :: Board -> Word64 -> Bool
isKing b i = k b .&. i /= 0

upLeft :: Word64 -> Word64
upLeft x = unsafeShiftL x 1

upRight :: Word64 -> Word64
upRight x = unsafeShiftL x 9

downRight :: Word64 -> Word64
downRight x = unsafeShiftR x 1

downLeft :: Word64 -> Word64
downLeft x = unsafeShiftR x 9


movePiece :: Word64 -> Move -> Word64
movePiece x move = (x `xor` src move) .|. dst move

doMove :: Board -> Move -> Player -> Board
doMove board move player
  | player == White = board {wp = whitePieces, k = kings}
  | otherwise = board {bp = blackPieces, k = kings}
  where
    whitePieces = movePiece (wp board) move
    blackPieces = movePiece (bp board) move
    kings = if isKing board . src $ move then movePiece (k board) move else k board


---------------------- display -------------------------

initialBoard :: Board
initialBoard =
  let
    white = field_1 .|. field_2 .|. field_3 .|. field_4 .|. field_5 .|. field_6 .|. field_7 .|. field_8 .|. field_9 .|. field_10 .|. field_11 .|. field_12
    black = field_32 .|. field_31 .|. field_30 .|. field_29 .|. field_28 .|. field_27 .|. field_26 .|. field_25 .|. field_24 .|. field_23 .|. field_22 .|. field_21
    kings = 0
  in Board {wp = white, bp = black, k = kings}

printBoard :: Board -> IO () -- prints board
printBoard board = putStr . unlines . (++["  1 2 3 4 5 6 7 8"])  . zipWith (++) (map (:" ") ['8','7'..'1']) . chunksOf 16 . intersperse ' ' . map (getFigureChar . getPiece board . mapBoard) $ [0..63]


mergeMoves :: MoveList -> [Word64]
mergeMoves m = nub $ map dst m

printBoardWithMoves :: Board -> MoveList -> IO () -- prints board
printBoardWithMoves board m = putStr . unlines . (++["  1 2 3 4 5 6 7 8"])  . zipWith (++) (map (:" ") ['8','7'..'1']) 
                              . chunksOf 16 . intersperse ' ' . map (\x -> getFigureChar $ if (mapBoard $ fromIntegral x) `elem` (mergeMoves m) then Special else (getPiece board (mapBoard $ fromIntegral x))) $ [0..63]



printBoardWithJumps :: Board -> Jump -> IO () -- prints board
printBoardWithJumps board m = putStr . unlines . (++["  1 2 3 4 5 6 7 8"])  . zipWith (++) (map (:" ") ['8','7'..'1']) 
                              . chunksOf 16 . intersperse ' ' . map (\x -> 
                                                                    if (mapBoard $ fromIntegral x) `elem` (jumpPath m) 
                                                                    then (chr . (+49) . fromJust . (elemIndex . mapBoard $ fromIntegral x) . jumpPath $ m) 
                                                                    else (getFigureChar $ getPiece board (mapBoard $ fromIntegral x))) 
                              $ [0..63]

getPiece :: Board -> Word64 -> Figure -- get piece [0..64]
getPiece b val
  | val == invalid = Empty
  | isBlack && isKing b val = BlackKing
  | isBlack = BlackPiece
  | isWhite && isKing b val = WhiteKing
  | isWhite = WhitePiece
  | otherwise = Empty
  where
    isBlack = bp b .&. val /= 0
    isWhite = wp b .&. val /= 0

getFigureChar :: Figure -> Char
getFigureChar x
  | x == WhitePiece = 'w'
  | x == BlackPiece = 'b'
  | x == BlackKing = 'B'
  | x == WhiteKing = 'W'
  | x == Special = 'x'
  | otherwise = '.'
