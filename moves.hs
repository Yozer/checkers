module Moves where

import           Board
import           Data.Bits
import           Data.Word
import           Masks

----------- PUBLIC METHODS ------------


getWhiteMovesList :: Board -> MoveData -- return list of moves
getWhiteMovesList board =
  let
    movers = getWhiteMovers board
    moveList = getWhiteMoveListRecur board movers
  in MoveData {moves = moveList, movesCount = length moveList}


getWhiteJumpsList :: Board -> JumpData
getWhiteJumpsList board =
  let
    jumpers = getWhiteJumpers board
    jumpList =  getJumpsListRecur board (empty board) jumpers (bp board)
  in JumpData {jumps = jumpList, jumpsCount = length jumpList}


getBlackMovesList :: Board -> MoveData -- return list of moves
getBlackMovesList board =
  let
    movers = getBlackMovers board
    moveList = getBlackMoveListRecur board movers
  in MoveData {moves = moveList, movesCount = length moveList}


getBlackJumpsList :: Board -> JumpData
getBlackJumpsList board =
  let
    jumpers = getBlackJumpers board
    jumpList = getJumpsListRecur board (empty board) jumpers (wp board)
  in JumpData {jumps = jumpList, jumpsCount = length jumpList}



---------- JUMP AND MOVE DETECTION ------------

getBlackMovers :: Board -> Word64 -- return pieces that can move
getBlackMovers board =
  let
    blackLeft = (upRight $ empty board) .&. bp board
    blackRight = (upLeft $ empty board) .&. bp board
    kingsMoves = ((downRight $ empty board) .&. blackKings board) .|. ((downLeft $ empty board) .&. blackKings board)
  in blackLeft .|. blackRight .|. kingsMoves


getBlackJumpers :: Board -> Word64 -- return pieces that can jump
getBlackJumpers board = getJumpersKings (empty board) (blackKings board) (wp board) .|. getJumpersPieces (empty board) (bp board) (wp board)

getWhiteMovers :: Board -> Word64 -- return pieces that can move
getWhiteMovers board =
  let
    whiteLeft = (downRight $ empty board) .&. wp board
    whiteRight = (downLeft $ empty board) .&. wp board
    kingsMoves = ((upRight $ empty board) .&. whiteKings board) .|. ((upLeft $ empty board) .&. whiteKings board)
  in whiteLeft .|. whiteRight .|. kingsMoves


getWhiteJumpers :: Board -> Word64 -- return pieces that can jump
getWhiteJumpers board = getJumpersKings (empty board) (whiteKings board) (bp board) .|. getJumpersPieces (empty board) (wp board) (bp board)


getJumpersPieces :: Word64 -> Word64 -> Word64 -> Word64 -- return pieces that can jump
getJumpersPieces empt me opponent =
  let
    leftUp = (downRight ((downRight $ empt) .&. opponent)) .&. me
    rightUp = (downLeft ((downLeft $ empt) .&. opponent)) .&. me
    leftDown = (upRight ((upRight $ empt) .&. opponent)) .&. me
    rightDown = (upLeft ((upLeft $ empt) .&. opponent)) .&. me
  in leftUp .|. rightUp .|. leftDown .|. rightDown


getJumpersKings :: Word64 -> Word64 -> Word64 -> Word64 -- return kings that can jump
getJumpersKings empt me opponent =
    let
      enemyUpLeft = (upLeft empt) .&. opponent
      enemyUpRight = (upRight empt) .&. opponent
      enemyDownRight = (downRight empt) .&. opponent
      enemyDownLeft = (downLeft empt) .&. opponent
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

getBlackMoveListRecur :: Board -> Word64 -> MoveList
getBlackMoveListRecur board movers
  | movers /= 0 = movesList ++ getBlackMoveListRecur board movers'
  | otherwise = []
  where
    pieceIndex = s $ getIndex movers
    movers' = movers `xor` pieceIndex
    movesList = if isKing board pieceIndex then getMovesListKings (empty board) pieceIndex pieceIndex pieceIndex pieceIndex pieceIndex else getBlackMovesListPieces board pieceIndex


getBlackMovesListPieces :: Board -> Word64 -> MoveList
getBlackMovesListPieces board index =
  let
    leftDown = downLeft index .&. empty board
    rightDown = downRight index .&. empty board
    result = if leftDown /= 0 then [Move {src = index, dst = downLeft index}] else []
    result' = if rightDown /= 0 then Move {src = index, dst = downRight index}:result else result
  in result'


getWhiteMoveListRecur :: Board -> Word64 -> MoveList
getWhiteMoveListRecur board movers
  | movers /= 0 = movesList ++ getWhiteMoveListRecur board movers'
  | otherwise = []
  where
    pieceIndex = s $ getIndex movers
    movers' = movers `xor` pieceIndex
    movesList = if isKing board pieceIndex then getMovesListKings (empty board) pieceIndex pieceIndex pieceIndex pieceIndex pieceIndex else getWhiteMovesListPieces board pieceIndex


getWhiteMovesListPieces :: Board -> Word64 -> MoveList
getWhiteMovesListPieces board index =
  let
    leftUp = upLeft index .&. empty board
    rightUp = upRight index .&. empty board
    result = if leftUp /= 0 then [Move {src = index, dst = upLeft index}] else []
    result' = if rightUp /= 0 then Move {src = index, dst = upRight index}:result else result
  in result'


--------------- MOVES KINGS ---------------

getMovesListKings :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> MoveList
getMovesListKings empt source leftUp rightUp leftDown rightDown
  | (leftUp .|. rightUp .|. leftDown .|. rightDown) == 0 = result4
  | otherwise = result4 ++ getMovesListKings empt source leftUp' rightUp' leftDown' rightDown'
  where
    leftUp' = upLeft leftUp .&. empt
    rightUp' = upRight rightUp .&. empt
    leftDown' = downLeft leftDown .&. empt
    rightDown' = downRight rightDown .&. empt
    result1 = if leftUp' /= 0 then [Move {src = source, dst = leftUp'}] else []
    result2 = if rightUp' /= 0 then Move {src = source, dst = rightUp'}:result1 else result1
    result3 = if leftDown'/= 0 then Move {src = source, dst = leftDown'}:result2 else result2
    result4 = if rightDown' /= 0 then Move {src = source, dst = rightDown'}:result3 else result3



---------- JUMPS  ------------
getJumpsListRecur :: Board -> Word64 -> Word64 -> Word64 -> JumpList
getJumpsListRecur board empt jumpers opponent
  | jumpers /= 0 = jumpList ++ getJumpsListRecur board empt jumpers' opponent
  | otherwise = []
  where
    pieceIndex = s $ getIndex jumpers
    jumpers' = jumpers `xor` pieceIndex
    jumpList = if isKing board pieceIndex 
              then map (\x -> Jump {jumpPath = x, jumpSize = length x - 1}) $ getJumpsListKings empt opponent pieceIndex
              else map (\x -> Jump {jumpPath = pieceIndex:x, jumpSize = length x}) $ getJumpsListPieces empt opponent pieceIndex

----------- JUMPS PIECES -------------

getJumpsListPieces :: Word64 -> Word64 -> Word64 -> [[Word64]] -- return list of pieces that can jump
getJumpsListPieces empt whitePieces piece =
  let
    leftUp = upLeft (upLeft piece .&. whitePieces) .&. empt
    rightUp = upRight (upRight piece .&. whitePieces) .&. empt
    rightDown = downRight (downRight piece .&. whitePieces) .&. empt
    leftDown = downLeft (downLeft piece .&. whitePieces) .&. empt

    newEmpty = empt .|. piece

    recurLeftUp = if leftUp /= 0 then map (leftUp:) $ getJumpsListPieces newEmpty ((upLeft piece) `xor` whitePieces) leftUp else []
    recurRightUp = if rightUp /= 0 then map (rightUp:) $ getJumpsListPieces newEmpty ((upRight piece) `xor` whitePieces) rightUp else []
    recurRightDown = if rightDown /= 0 then map (rightDown:) $ getJumpsListPieces newEmpty ((downRight piece) `xor` whitePieces) rightDown else []
    recurLeftDown =  if leftDown /= 0 then map (leftDown:) $ getJumpsListPieces newEmpty ((downLeft piece) `xor` whitePieces) leftDown else []
    result = recurLeftUp ++ recurRightUp ++ recurRightDown ++ recurLeftDown
  in if result == [] then [[]] else result



getJumpsListKings :: Word64 -> Word64 -> Word64 -> [[Word64]] -- return list of kings that can jump
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