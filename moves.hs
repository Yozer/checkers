module Moves where

import           Board
import           Data.Bits
import           Data.Word
import           Masks
import Data.List
import Data.Function

----------- PUBLIC METHODS ------------

getActions :: Board -> Player -> [MoveHolder]
getActions board player = if not . null $ jumps' then jumps' else moves'
  where
    moves' = map NormalMove $ getMoves board player
    jumps' = map JumpMove . filterJumps $ getJumps board player

filterJumps :: MovesList -> MovesList -- remove short jumps
filterJumps jumps
  | null jumps = []
  | otherwise = result
  where
    max' = length . maximumBy (compare `on` length) $ jumps
    result = filter (\x -> length x == max') jumps

getMoves :: Board -> Player -> MovesList
getMoves board player
  | player == White = getWhiteMovesList board
  | otherwise = getBlackMovesList board

getJumps :: Board -> Player -> MovesList
getJumps board player
  | player == White = getWhiteJumpsList board
  | otherwise = getBlackJumpsList board


-- METHODS -- 


getWhiteMovesList :: Board -> MovesList -- return list of moves
getWhiteMovesList board =
  let
    movers = getWhiteMovers board
    moveList = getWhiteMoveListRecur board movers
  in moveList


getWhiteJumpsList :: Board -> MovesList
getWhiteJumpsList board =
  let
    jumpers = getWhiteJumpers board
    jumpList =  getJumpsListRecur board (empty board) jumpers (bp board)
  in jumpList


getBlackMovesList :: Board -> MovesList -- return list of moves
getBlackMovesList board =
  let
    movers = getBlackMovers board
    moveList = getBlackMoveListRecur board movers
  in moveList


getBlackJumpsList :: Board -> MovesList
getBlackJumpsList board =
  let
    jumpers = getBlackJumpers board
    jumpList = getJumpsListRecur board (empty board) jumpers (wp board)
  in jumpList



---------- JUMP AND MOVE DETECTION ------------

getBlackMovers :: Board -> Word64 -- return pieces that can move
getBlackMovers board =
  let
    blackLeft = upRight (empty board) .&. bp board
    blackRight = upLeft (empty board) .&. bp board
    kingsMoves = (downRight (empty board) .&. blackKings board) .|. (downLeft (empty board) .&. blackKings board)
  in blackLeft .|. blackRight .|. kingsMoves


getBlackJumpers :: Board -> Word64 -- return pieces that can jump
getBlackJumpers board = getJumpersKings (empty board) (blackKings board) (wp board) .|. getJumpersPieces (empty board) (bp board) (wp board)

getWhiteMovers :: Board -> Word64 -- return pieces that can move
getWhiteMovers board =
  let
    whiteLeft = downRight (empty board) .&. wp board
    whiteRight = downLeft (empty board) .&. wp board
    kingsMoves = (upRight (empty board) .&. whiteKings board) .|. (upLeft (empty board) .&. whiteKings board)
  in whiteLeft .|. whiteRight .|. kingsMoves


getWhiteJumpers :: Board -> Word64 -- return pieces that can jump
getWhiteJumpers board = getJumpersKings (empty board) (whiteKings board) (bp board) .|. getJumpersPieces (empty board) (wp board) (bp board)


getJumpersPieces :: Word64 -> Word64 -> Word64 -> Word64 -- return pieces that can jump
getJumpersPieces empt me opponent =
  let
    leftUp = downRight (downRight empt .&. opponent) .&. me
    rightUp = downLeft (downLeft empt .&. opponent) .&. me
    leftDown = upRight (upRight empt .&. opponent) .&. me
    rightDown = upLeft (upLeft empt .&. opponent).&. me
  in leftUp .|. rightUp .|. leftDown .|. rightDown


getJumpersKings :: Word64 -> Word64 -> Word64 -> Word64 -- return kings that can jump
getJumpersKings empt me opponent =
    let
      enemyUpLeft = upLeft empt .&. opponent
      enemyUpRight = upRight empt .&. opponent
      enemyDownRight = downRight empt .&. opponent
      enemyDownLeft = downLeft empt .&. opponent
    in getJumpersKingsRecur empt enemyUpLeft enemyUpRight enemyDownLeft enemyDownRight me 0


getJumpersKingsRecur :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64
getJumpersKingsRecur empt enemyUpLeft enemyUpRight enemyDownLeft enemyDownRight kings result
    | (enemyUpLeft .|.  enemyUpRight .|. enemyDownLeft .|. enemyDownRight) == 0 = result -- no moves possible
    | otherwise = getJumpersKingsRecur empt newEnemyUpLeft newEnemyUpRight newEnemyDownLeft newEnemyDownRight kings (matchedKings .|. result)
    where
      enemyUpRight' = upRight enemyUpRight
      enemyUpLeft' = upLeft enemyUpLeft
      enemyDownLeft' = downLeft enemyDownLeft
      enemyDownRight' = downRight enemyDownRight
      matchedKings = kings .&. (enemyUpRight' .|. enemyUpLeft' .|. enemyDownLeft' .|. enemyDownRight') -- kings that can jump
      newEnemyUpRight = enemyUpRight' .&. empt
      newEnemyUpLeft = enemyUpLeft' .&. empt
      newEnemyDownRight = enemyDownRight' .&. empt
      newEnemyDownLeft = enemyDownLeft' .&. empt

----------- MOVES PIECES -------------

getBlackMoveListRecur :: Board -> Word64 -> MovesList
getBlackMoveListRecur board movers
  | movers /= 0 = movesList ++ getBlackMoveListRecur board movers'
  | otherwise = []
  where
    pieceIndex = s $ getIndex movers
    movers' = movers `xor` pieceIndex
    movesList = if isKing board pieceIndex then getMovesListKings (empty board) pieceIndex pieceIndex pieceIndex pieceIndex pieceIndex else getBlackMovesListPieces board pieceIndex


getBlackMovesListPieces :: Board -> Word64 -> MovesList
getBlackMovesListPieces board index =
  let
    leftDown = downLeft index .&. empty board
    rightDown = downRight index .&. empty board
    result = [[index, downLeft index] | leftDown /= 0]
    result' = if rightDown /= 0 then [index, downRight index]:result else result
  in result'


getWhiteMoveListRecur :: Board -> Word64 -> MovesList
getWhiteMoveListRecur board movers
  | movers /= 0 = movesList ++ getWhiteMoveListRecur board movers'
  | otherwise = []
  where
    pieceIndex = s $ getIndex movers
    movers' = movers `xor` pieceIndex
    movesList = if isKing board pieceIndex then getMovesListKings (empty board) pieceIndex pieceIndex pieceIndex pieceIndex pieceIndex else getWhiteMovesListPieces board pieceIndex


getWhiteMovesListPieces :: Board -> Word64 -> MovesList
getWhiteMovesListPieces board index =
  let
    leftUp = upLeft index .&. empty board
    rightUp = upRight index .&. empty board
    result = [[index, upLeft index] | leftUp /= 0]
    result' = if rightUp /= 0 then [index, upRight index]:result else result
  in result'


--------------- MOVES KINGS ---------------

getMovesListKings :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> MovesList
getMovesListKings empt source leftUp rightUp leftDown rightDown
  | (leftUp .|. rightUp .|. leftDown .|. rightDown) == 0 = result4
  | otherwise = result4 ++ getMovesListKings empt source leftUp' rightUp' leftDown' rightDown'
  where
    leftUp' = upLeft leftUp .&. empt
    rightUp' = upRight rightUp .&. empt
    leftDown' = downLeft leftDown .&. empt
    rightDown' = downRight rightDown .&. empt
    result1 = [[source, leftUp'] | leftUp' /= 0]
    result2 = if rightUp' /= 0 then [source, rightUp']:result1 else result1
    result3 = if leftDown'/= 0 then [source, leftDown']:result2 else result2
    result4 = if rightDown' /= 0 then [source, rightDown']:result3 else result3



---------- JUMPS  ------------
getJumpsListRecur :: Board -> Word64 -> Word64 -> Word64 -> MovesList
getJumpsListRecur board empt jumpers opponent
  | jumpers /= 0 = jumpList ++ getJumpsListRecur board empt jumpers' opponent
  | otherwise = []
  where
    pieceIndex = s $ getIndex jumpers
    jumpers' = jumpers `xor` pieceIndex
    jumpList = if isKing board pieceIndex 
              then getJumpsListKings empt opponent pieceIndex
              else getJumpsListPieces empt opponent pieceIndex

----------- JUMPS PIECES -------------

getJumpsListPieces :: Word64 -> Word64 -> Word64 -> [[Word64]] -- return list of pieces that can jump
getJumpsListPieces empt opponent piece =
  let
    leftUp = upLeft (upLeft piece .&. opponent) .&. empt
    rightUp = upRight (upRight piece .&. opponent) .&. empt
    rightDown = downRight (downRight piece .&. opponent) .&. empt
    leftDown = downLeft (downLeft piece .&. opponent) .&. empt
    
    newEmpty = empt .|. piece

    leftUpNewOpponent = if leftUp == 0 then 0 else (downRight leftUp) `xor` opponent
    rightUpNewOpponent = if rightUp == 0 then 0 else (downLeft rightUp) `xor` opponent
    rightDownNewOpponent = if rightDown == 0 then 0 else (upLeft rightDown) `xor` opponent
    leftDownNewOpponent = if leftDown == 0 then 0 else (upRight leftDown) `xor` opponent

    recurLeftUp = if leftUp == 0 then [] else getJumpsListPieces newEmpty leftUpNewOpponent leftUp
    recurRightUp = if rightUp == 0 then [] else getJumpsListPieces newEmpty rightUpNewOpponent rightUp
    recurRightDown = if rightDown == 0 then [] else getJumpsListPieces newEmpty rightDownNewOpponent rightDown
    recurLeftDown = if leftDown == 0 then [] else getJumpsListPieces newEmpty leftDownNewOpponent leftDown

    recurLeftUp' = map (piece:) $ if leftUp /= 0 && null recurLeftUp then [[leftUp]] else recurLeftUp
    recurRightUp' = map (piece:) $ if rightUp /= 0 && null recurRightUp then [[rightUp]] else recurRightUp
    recurRightDown' = map (piece:) $ if rightDown /= 0 && null recurRightDown  then [[rightDown]] else recurRightDown
    recurLeftDown' = map (piece:) $ if leftDown /= 0 && null recurLeftDown  then [[leftDown]] else recurLeftDown
    result = recurLeftUp' ++ recurRightUp' ++ recurRightDown' ++ recurLeftDown'
  in result



getJumpsListKings :: Word64 -> Word64 -> Word64 -> MovesList -- return list of kings that can jump
getJumpsListKings empt opponent piece =
  let
    leftUp = moveUntilOpponent piece empt opponent upLeft
    rightUp = moveUntilOpponent piece empt opponent upRight
    rightDown = moveUntilOpponent piece empt opponent downRight
    leftDown = moveUntilOpponent piece empt opponent downLeft

    newEmpty = empt .|. piece
    leftUpNewBlack = if leftUp == [] then 0 else (downRight . head $ leftUp) `xor` opponent
    rightUpNewBlack = if rightUp == [] then 0 else (downLeft . head $ rightUp) `xor` opponent
    rightDownNewBlack = if rightDown == [] then 0 else (upLeft . head $ rightDown) `xor` opponent
    leftDownNewBlack = if leftDown == [] then 0 else (upRight . head $ leftDown) `xor` opponent

    recurLeftUp = concatMap (getJumpsListKings newEmpty leftUpNewBlack) leftUp
    recurRightUp = concatMap (getJumpsListKings newEmpty rightUpNewBlack) rightUp
    recurRightDown = concatMap (getJumpsListKings newEmpty rightDownNewBlack) rightDown
    recurLeftDown = concatMap (getJumpsListKings newEmpty leftDownNewBlack) leftDown

    recurLeftUp' = if recurLeftUp == [] then map (\x -> piece:x:[]) leftUp else map (piece:) recurLeftUp
    recurRightUp' = if recurRightUp == [] then map (\x -> piece:x:[]) rightUp else map (piece:) recurRightUp
    recurRightDown' = if recurRightDown == [] then map (\x -> piece:x:[]) rightDown else map (piece:) recurRightDown
    recurLeftDown' = if recurLeftDown == [] then map (\x -> piece:x:[]) leftDown else map (piece:) recurLeftDown

    result = recurLeftUp' ++ recurRightUp' ++ recurRightDown' ++ recurLeftDown'
  in result



getAllFreeSquares :: Word64 -> Word64 -> (Word64 -> Word64) -> [Word64]
getAllFreeSquares piece empt moveFunction
  | nextEmptySquare == 0 = [piece]
  | otherwise = piece:getAllFreeSquares nextEmptySquare empt moveFunction
  where
    nextEmptySquare = moveFunction piece .&. empt


moveUntilOpponent ::  Word64 -> Word64 -> Word64 -> (Word64 -> Word64) -> [Word64]
moveUntilOpponent piece empt opponent moveFunction
  -- we have a jump? return all possible endings for this jump
  | isNextMoveAJump /= 0 = getAllFreeSquares isNextMoveAJump empt moveFunction
  -- if we don't have a jump, can we move a to the next square?
  | otherwise = if nextSquare /= 0 then moveUntilOpponent nextSquare empt opponent moveFunction else []
  where
    isNextMoveAJump = moveFunction (moveFunction piece .&. opponent) .&. empt
    nextSquare = moveFunction piece .&. empt