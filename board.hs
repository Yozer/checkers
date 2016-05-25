module Board where

import           Data.Bits
import           Data.List
import           Data.List.Split
import           Data.Word
import           Masks
import Debug.Trace

data Figure = BlackPiece | WhitePiece | BlackKing | WhiteKing | Empty | Special deriving (Show, Eq)
data Player = White | Black deriving(Show, Eq)

data Board = Board {
  wp :: Word64,
  bp :: Word64,
  k  :: Word64
} deriving (Show, Eq)

type Path = [Word64]
type MovesList = [Path]

data MoveHolder =  None | JumpMove Path | NormalMove Path deriving(Show, Eq, Ord)

------ HELPERS
empty :: Board -> Word64
empty b = (complement (bp b .|. wp b)) .&. bitsOnTheBoard

whiteKings :: Board -> Word64
whiteKings b = k b .&. wp b

blackKings :: Board -> Word64
blackKings b = k b .&. bp b

blackPieces :: Board -> Word64
blackPieces b = (complement $ k b) .&. bp b

whitePieces :: Board -> Word64
whitePieces b = (complement $ k b) .&. wp b

isKing :: Board -> Word64 -> Bool
isKing b i = k b .&. i /= 0

isWhiteKing :: Board -> Word64 -> Bool
isWhiteKing b i = k b .&. i /= 0 && wp b .&. i /= 0

isBlackKing :: Board -> Word64 -> Bool
isBlackKing b i = k b .&. i /= 0 && bp b .&. i /= 0


upLeft :: Word64 -> Word64
upLeft x = unsafeShiftL x 1

upRight :: Word64 -> Word64
upRight x = unsafeShiftL x 9

downRight :: Word64 -> Word64
downRight x = unsafeShiftR x 1

downLeft :: Word64 -> Word64
downLeft x = unsafeShiftR x 9


whiteBottomEdge :: Word64
whiteBottomEdge = mergeBoardFields [1..4]

blackBottomEdge :: Word64
blackBottomEdge = mergeBoardFields [29..32]

-- END OF HELPERS

--- MOVE AND JUMPS EVALUATION

movePiece :: Word64 -> Word64 -> Word64 -> Word64 -- make a normal move, no validation performed
movePiece x from to = (x `xor` from) .|. to

promotePiece :: Word64 -> Player -> Word64 -> Word64 -- promote piece if needed
promotePiece dst player kings
  | (player == White && dst .&. blackBottomEdge /= 0) || (player == Black && dst .&. whiteBottomEdge /= 0) = kings .|. dst
  | otherwise = kings


doMove :: Board -> Player -> MoveHolder -> Board
doMove board player (NormalMove move)
  | player == White = board {wp = whitePieces', k = kings'}
  | otherwise = board {bp = blackPieces', k = kings'}
  where
    from = head move
    to = last move
    whitePieces' = movePiece (wp board) from to -- move piece for white
    blackPieces' = movePiece (bp board) from to -- move piece for black
    kings = if isKing board from then movePiece (k board) from to else k board
    kings' = promotePiece to player kings


doMove board player (JumpMove jump) = doJump' board player jump
doMove board _ _ = board

doJump' :: Board -> Player -> Path -> Board
doJump' board player (a:b:path)
  | path == [] = result {k = promotedKings}
  | otherwise = doJump' result player (b:path)
  where
    direction = getMoveDirection a b
    opponent = if player == White then bp board else wp board
    killed = getKilledPieces opponent direction a
    whitePieces' = if player == White then (wp board `xor` a) .|. b else wp board `xor` killed
    blackPieces' = if player == Black then (bp board `xor` a) .|. b else bp board `xor` killed
    kings1 = (complement killed) .&. k board
    kings2 = if kings1 .&. a /= 0 then (kings1 `xor` a) .|. b else kings1 --move our piece that made that jump if he was a king
    result = Board {wp = whitePieces', bp = blackPieces', k = kings2}
    promotedKings = promotePiece b player (k result)

doJump' x _ _ = x

--- END OF MOVE AND JUMPS EVALUATION


-- check if game has ended
isGameEnded :: Board -> Bool
isGameEnded board = bp board == 0 || wp board == 0


------ HELPERS -----

getKilledPieces :: Word64 -> (Word64 -> Word64) -> Word64 -> Word64
getKilledPieces opponent direction position
  | killed /= 0 = killed
  | otherwise = getKilledPieces opponent direction (direction position)
  where
    killed = position .&. opponent

getMoveDirection :: Word64 -> Word64 -> (Word64 -> Word64)
getMoveDirection source destination
  | res >= (9 :: Int) = upRight
  | res > 0 = upLeft
  | res <= -9 = downLeft
  | otherwise = downRight
  where
    res = (fromIntegral $ getIndex destination) - (fromIntegral $ getIndex source)

---------------------- display -------------------------

initialBoard :: Board
initialBoard =
  let
    white = mergeBoardFields [1..12]
    black = mergeBoardFields [21..32]
    kings = 0
  in Board {wp = white, bp = black, k = kings}

printBoard :: Board -> IO () -- prints board
printBoard board = putStr . unlines . (++["  1 2 3 4 5 6 7 8"])  . zipWith (++) (map (:" ") ['8','7'..'1']) . chunksOf 16 . intersperse ' ' . map (getFigureChar . getPiece board . mapBoard) $ [0..63]


printBoardWithPath :: Board -> Path -> IO () -- prints board
printBoardWithPath board m = putStr . unlines . (++["  1 2 3 4 5 6 7 8"])  . zipWith (++) (map (:" ") ['8','7'..'1'])
                              . chunksOf 16 . intersperse ' ' . map (\x -> getFigureChar $ if (mapBoard $ fromIntegral x) `elem` m then Special else (getPiece board (mapBoard $ fromIntegral x))) $ ([0..63] :: [Int])



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
