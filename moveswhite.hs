module MovesWhite where

import           Board
import           Data.Bits
import           Data.Word
import           Masks

-- TODO performance?
-- TODO save what black pieces we have jumped over and save only src and dst? Have to wait for ptm to tell us what format of jumps we will have


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
    jumpList = getWhiteJumpsListRecur board jumpers
  in JumpData {jumps = jumpList, jumpsCount = length jumpList}


---------- JUMP AND MOVE DETECTION ------------

getWhiteMovers :: Board -> Word64 -- return pieces that can move
getWhiteMovers board =
  let
    whiteLeft = (downRight $ empty board) .&. wp board
    whiteRight = (downLeft $ empty board) .&. wp board
    kingsMoves = ((upRight $ empty board) .&. whiteKings board) .|. ((upLeft $ empty board) .&. whiteKings board)
  in whiteLeft .|. whiteRight .|. kingsMoves


getWhiteJumpers :: Board -> Word64 -- return pieces that can jump
getWhiteJumpers board = getWhiteJumpersKings board .|. getWhiteJumpersPieces board

----------- MOVES -------------

getWhiteMoveListRecur :: Board -> Word64 -> MoveList
getWhiteMoveListRecur board movers
  | movers /= 0 = movesList ++ getWhiteMoveListRecur board movers'
  | otherwise = []
  where
    pieceIndex = s $ getIndex movers
    movers' = movers `xor` pieceIndex
    movesList = if isKing board pieceIndex then getWhiteMovesListKings board pieceIndex pieceIndex pieceIndex pieceIndex pieceIndex else getWhiteMovesListPieces board pieceIndex


getWhiteMovesListPieces :: Board -> Word64 -> MoveList
getWhiteMovesListPieces board index =
  let
    leftUp = upLeft index .&. empty board
    rightUp = upRight index .&. empty board
    result = if leftUp /= 0 then [Move {src = index, dst = upLeft index}] else []
    result' = if rightUp /= 0 then Move {src = index, dst = upRight index}:result else result
  in result'

getWhiteMovesListKings :: Board -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> MoveList
getWhiteMovesListKings board source leftUp rightUp leftDown rightDown
  | (leftUp .|. rightUp .|. leftDown .|. rightDown) == 0 = result4
  | otherwise = result4 ++ getWhiteMovesListKings board source leftUp' rightUp' leftDown' rightDown'
  where
    leftUp' = upLeft leftUp .&. empty board
    rightUp' = upRight rightUp .&. empty board
    leftDown' = downLeft leftDown .&. empty board
    rightDown' = downRight rightDown .&. empty board
    result1 = if leftUp' /= 0 then [Move {src = source, dst = leftUp'}] else []
    result2 = if rightUp' /= 0 then Move {src = source, dst = rightUp'}:result1 else result1
    result3 = if leftDown'/= 0 then Move {src = source, dst = leftDown'}:result2 else result2
    result4 = if rightDown' /= 0 then Move {src = source, dst = rightDown'}:result3 else result3

---------- JUMPS ------------

getWhiteJumpersPieces :: Board -> Word64 -- return only pieces that can jump
getWhiteJumpersPieces board =
  let
    whiteLeftUp = (downRight ((downRight $ empty board) .&. (bp board))) .&. wp board
    whiteRightUp = (downLeft ((downLeft $ empty board) .&. (bp board))) .&. wp board
    whiteLeftDown = (upRight ((upRight $ empty board) .&. (bp board))) .&. wp board
    whiteRightDown = (upLeft ((upLeft $ empty board) .&. (bp board))) .&. wp board
  in (whiteRightUp .|. whiteLeftUp .|. whiteLeftDown .|. whiteRightDown) .&. (piecesOnly board)

getWhiteJumpersKings :: Board -> Word64 -- return only kings that can jump
getWhiteJumpersKings board =
    let
      enemyUpLeft = (upLeft $ empty board) .&. bp board
      enemyUpRight = (upRight $ empty board) .&. bp board
      enemyDownRight = (downRight $ empty board) .&. bp board
      enemyDownLeft = (downLeft $ empty board) .&. bp board
    in getWhiteJumpersKingsRecur (empty board) enemyUpLeft enemyUpRight enemyDownLeft enemyDownRight (whiteKings board) 0

getWhiteJumpersKingsRecur :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64
getWhiteJumpersKingsRecur empt enemyUpLeft enemyUpRight enemyDownLeft enemyDownRight kings result
    | (enemyUpLeft .|.  enemyUpRight .|. enemyDownLeft .|. enemyDownRight) == 0 = result -- no moves possible
    | otherwise = getWhiteJumpersKingsRecur empt newEnemyUpLeft newEnemyUpRight newEnemyDownLeft newEnemyDownRight kings (matchedKings .|. result)
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

------ Jump List ---------

getWhiteJumpsListRecur :: Board -> Word64 -> JumpList
getWhiteJumpsListRecur board jumpers
  | jumpers /= 0 = jumpList ++ getWhiteJumpsListRecur board jumpers'
  | otherwise = []
  where
    pieceIndex = s $ getIndex jumpers
    jumpers' = jumpers `xor` pieceIndex
    jumpList = if isKing board pieceIndex 
              then map (\x -> Jump {jumpPath = x, jumpSize = length x - 1}) $ getWhiteJumpsListKings (empty board) (bp board) pieceIndex
              else map (\x -> Jump {jumpPath = pieceIndex:x, jumpSize = length x}) $ getWhiteJumpsListPieces (empty board) (bp board) pieceIndex

getWhiteJumpsListPieces :: Word64 -> Word64 -> Word64 -> [[Word64]] -- return list of pieces that can jump
getWhiteJumpsListPieces empt blackPieces piece =
  let
    leftUp = upLeft (upLeft piece .&. blackPieces) .&. empt
    rightUp = upRight (upRight piece .&. blackPieces) .&. empt
    rightDown = downRight (downRight piece .&. blackPieces) .&. empt
    leftDown = downLeft (downLeft piece .&. blackPieces) .&. empt

    newEmpty = empt .|. piece

    recurLeftUp = if leftUp /= 0 then map (leftUp:) $ getWhiteJumpsListPieces newEmpty ((upLeft piece) `xor` blackPieces) leftUp else []
    recurRightUp = if rightUp /= 0 then map (rightUp:) $ getWhiteJumpsListPieces newEmpty ((upRight piece) `xor` blackPieces) rightUp else []
    recurRightDown = if rightDown /= 0 then map (rightDown:) $ getWhiteJumpsListPieces newEmpty ((downRight piece) `xor` blackPieces) rightDown else []
    recurLeftDown =  if leftDown /= 0 then map (leftDown:) $ getWhiteJumpsListPieces newEmpty ((downLeft piece) `xor` blackPieces) leftDown else []
    result = recurLeftUp ++ recurRightUp ++ recurRightDown ++ recurLeftDown
  in if result == [] then [[]] else result


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

getWhiteJumpsListKings :: Word64 -> Word64 -> Word64 -> [[Word64]] -- return list of kings that can jump
getWhiteJumpsListKings empt blackPieces piece =
  let
    leftUp = moveUntilOpponent piece empt blackPieces upLeft
    rightUp = moveUntilOpponent piece empt blackPieces upRight
    rightDown = moveUntilOpponent piece empt blackPieces downRight
    leftDown = moveUntilOpponent piece empt blackPieces downLeft

    newEmpty = empt .|. piece
    leftUpNewBlack = if leftUp == [] then 0 else (downRight . head $ leftUp) `xor` blackPieces
    rightUpNewBlack = if rightUp == [] then 0 else (downLeft . head $ rightUp) `xor` blackPieces
    rightDownNewBlack = if rightDown == [] then 0 else (upLeft . head $ rightDown) `xor` blackPieces
    leftDownNewBlack = if leftDown == [] then 0 else (upRight . head $ leftDown) `xor` blackPieces

    recurLeftUp = concatMap (getWhiteJumpsListKings newEmpty leftUpNewBlack) leftUp
    recurRightUp = concatMap (getWhiteJumpsListKings newEmpty rightUpNewBlack) rightUp
    recurRightDown = concatMap (getWhiteJumpsListKings newEmpty rightDownNewBlack) rightDown
    recurLeftDown = concatMap (getWhiteJumpsListKings newEmpty leftDownNewBlack) leftDown

    recurLeftUp' = if recurLeftUp == [] then map (\x -> piece:x:[]) leftUp else map (piece:) recurLeftUp
    recurRightUp' = if recurRightUp == [] then map (\x -> piece:x:[]) rightUp else map (piece:) recurRightUp
    recurRightDown' = if recurRightDown == [] then map (\x -> piece:x:[]) rightDown else map (piece:) recurRightDown
    recurLeftDown' = if recurLeftDown == [] then map (\x -> piece:x:[]) leftDown else map (piece:) recurLeftDown

    result = recurLeftUp' ++ recurRightUp' ++ recurRightDown' ++ recurLeftDown'
  in result