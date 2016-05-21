module MovesBlack where

import           Board
import           Data.Bits
import           Data.Word
import           Masks

-- TODO performance?
-- TODO save what black pieces we have jumped over and save only src and dst? Have to wait for ptm to tell us what format of jumps we will have


----------- PUBLIC METHODS ------------


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
    jumpList = getBlackJumpsListRecur board jumpers
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
getBlackJumpers board = getBlackJumpersKings board .|. getBlackJumpersPieces board

----------- MOVES -------------

getBlackMoveListRecur :: Board -> Word64 -> MoveList
getBlackMoveListRecur board movers
  | movers /= 0 = movesList ++ getBlackMoveListRecur board movers'
  | otherwise = []
  where
    pieceIndex = s $ getIndex movers
    movers' = movers `xor` pieceIndex
    movesList = if isKing board pieceIndex then getBlackMovesListKings board pieceIndex pieceIndex pieceIndex pieceIndex pieceIndex else getBlackMovesListPieces board pieceIndex


getBlackMovesListPieces :: Board -> Word64 -> MoveList
getBlackMovesListPieces board index =
  let
    leftDown = downLeft index .&. empty board
    rightDown = downRight index .&. empty board
    result = if leftDown /= 0 then [Move {src = index, dst = downLeft index}] else []
    result' = if rightDown /= 0 then Move {src = index, dst = downRight index}:result else result
  in result'

getBlackMovesListKings :: Board -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> MoveList
getBlackMovesListKings board source leftUp rightUp leftDown rightDown
  | (leftUp .|. rightUp .|. leftDown .|. rightDown) == 0 = result4
  | otherwise = result4 ++ getBlackMovesListKings board source leftUp' rightUp' leftDown' rightDown'
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

getBlackJumpersPieces :: Board -> Word64 -- return only pieces that can jump
getBlackJumpersPieces board =
  let
    blackLeftUp = (downRight ((downRight $ empty board) .&. (wp board))) .&. bp board
    blackRightUp = (downLeft ((downLeft $ empty board) .&. (wp board))) .&. bp board
    blackLeftDown = (upRight ((upRight $ empty board) .&. (wp board))) .&. bp board
    blackRightDown = (upLeft ((upLeft $ empty board) .&. (wp board))) .&. bp board
  in (blackRightUp .|. blackLeftUp .|. blackLeftDown .|. blackRightDown) .&. (piecesOnly board)

getBlackJumpersKings :: Board -> Word64 -- return only kings that can jump
getBlackJumpersKings board =
    let
      enemyUpLeft = (upLeft $ empty board) .&. wp board
      enemyUpRight = (upRight $ empty board) .&. wp board
      enemyDownRight = (downRight $ empty board) .&. wp board
      enemyDownLeft = (downLeft $ empty board) .&. wp board
    in getBlackJumpersKingsRecur (empty board) enemyUpLeft enemyUpRight enemyDownLeft enemyDownRight (blackKings board) 0

getBlackJumpersKingsRecur :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> Word64
getBlackJumpersKingsRecur empt enemyUpLeft enemyUpRight enemyDownLeft enemyDownRight kings result
    | (enemyUpLeft .|.  enemyUpRight .|. enemyDownLeft .|. enemyDownRight) == 0 = result -- no moves possible
    | otherwise = getBlackJumpersKingsRecur empt newEnemyUpLeft newEnemyUpRight newEnemyDownLeft newEnemyDownRight kings (matchedKings .|. result)
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

getBlackJumpsListRecur :: Board -> Word64 -> JumpList
getBlackJumpsListRecur board jumpers
  | jumpers /= 0 = jumpList ++ getBlackJumpsListRecur board jumpers'
  | otherwise = []
  where
    pieceIndex = s $ getIndex jumpers
    jumpers' = jumpers `xor` pieceIndex
    jumpList = if isKing board pieceIndex 
              then map (\x -> Jump {jumpPath = x, jumpSize = length x - 1}) $ getBlackJumpsListKings (empty board) (wp board) pieceIndex
              else map (\x -> Jump {jumpPath = pieceIndex:x, jumpSize = length x}) $ getBlackJumpsListPieces (empty board) (wp board) pieceIndex

getBlackJumpsListPieces :: Word64 -> Word64 -> Word64 -> [[Word64]] -- return list of pieces that can jump
getBlackJumpsListPieces empt whitePieces piece =
  let
    leftUp = upLeft (upLeft piece .&. whitePieces) .&. empt
    rightUp = upRight (upRight piece .&. whitePieces) .&. empt
    rightDown = downRight (downRight piece .&. whitePieces) .&. empt
    leftDown = downLeft (downLeft piece .&. whitePieces) .&. empt

    newEmpty = empt .|. piece

    recurLeftUp = if leftUp /= 0 then map (leftUp:) $ getBlackJumpsListPieces newEmpty ((upLeft piece) `xor` whitePieces) leftUp else []
    recurRightUp = if rightUp /= 0 then map (rightUp:) $ getBlackJumpsListPieces newEmpty ((upRight piece) `xor` whitePieces) rightUp else []
    recurRightDown = if rightDown /= 0 then map (rightDown:) $ getBlackJumpsListPieces newEmpty ((downRight piece) `xor` whitePieces) rightDown else []
    recurLeftDown =  if leftDown /= 0 then map (leftDown:) $ getBlackJumpsListPieces newEmpty ((downLeft piece) `xor` whitePieces) leftDown else []
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

getBlackJumpsListKings :: Word64 -> Word64 -> Word64 -> [[Word64]] -- return list of kings that can jump
getBlackJumpsListKings empt whitePieces piece =
  let
    leftUp = moveUntilOpponent piece empt whitePieces upLeft
    rightUp = moveUntilOpponent piece empt whitePieces upRight
    rightDown = moveUntilOpponent piece empt whitePieces downRight
    leftDown = moveUntilOpponent piece empt whitePieces downLeft

    newEmpty = empt .|. piece
    leftUpNewBlack = if leftUp == [] then 0 else (downRight . head $ leftUp) `xor` whitePieces
    rightUpNewBlack = if rightUp == [] then 0 else (downLeft . head $ rightUp) `xor` whitePieces
    rightDownNewBlack = if rightDown == [] then 0 else (upLeft . head $ rightDown) `xor` whitePieces
    leftDownNewBlack = if leftDown == [] then 0 else (upRight . head $ leftDown) `xor` whitePieces

    recurLeftUp = concatMap (getBlackJumpsListKings newEmpty leftUpNewBlack) leftUp
    recurRightUp = concatMap (getBlackJumpsListKings newEmpty rightUpNewBlack) rightUp
    recurRightDown = concatMap (getBlackJumpsListKings newEmpty rightDownNewBlack) rightDown
    recurLeftDown = concatMap (getBlackJumpsListKings newEmpty leftDownNewBlack) leftDown

    recurLeftUp' = if recurLeftUp == [] then map (\x -> piece:x:[]) leftUp else map (piece:) recurLeftUp
    recurRightUp' = if recurRightUp == [] then map (\x -> piece:x:[]) rightUp else map (piece:) recurRightUp
    recurRightDown' = if recurRightDown == [] then map (\x -> piece:x:[]) rightDown else map (piece:) recurRightDown
    recurLeftDown' = if recurLeftDown == [] then map (\x -> piece:x:[]) leftDown else map (piece:) recurLeftDown

    result = recurLeftUp' ++ recurRightUp' ++ recurRightDown' ++ recurLeftDown'
  in result