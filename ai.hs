module Ai where

import           Board
import           Data.List
import           Eval
import           Moves
import Control.Parallel.Strategies

data MoveHolder =  None | JumpMove Jump | NormalMove Move deriving(Show, Eq, Ord)
type AlphaResult = (Float, MoveHolder)

maxEval :: Float
maxEval = 401
maxDeep :: Int
maxDeep = 12

alphaBeta :: Board -> AlphaResult
alphaBeta board = maximum $  parMap rpar (\x -> (alphaBeta' (makeMove board White x) Black maxDeep (-maxEval) maxEval, x)) actions  --iterativeDeepening board actions ((-maxEval - 1), None)
  where
    actions = getActions board White


alphaBeta' :: Board -> Player -> Int -> Float -> Float -> Float
alphaBeta' board player depth alpha beta
  | depth == 0 || isGameEnded board || actions == [] = evaluate board player
  | otherwise = makeMoves actions board depth alpha beta player (-maxEval - 1)
  where
    actions = getActions board player


makeMove :: Board -> Player -> MoveHolder -> Board
makeMove board player (JumpMove x) = doJump board player x
makeMove board player (NormalMove x) = doMove board player x
makeMove board _ _ = board

makeMoves :: [MoveHolder] -> Board -> Int -> Float -> Float -> Player -> Float -> Float
makeMoves (move:restMoves) board depth alpha beta player best
  | restMoves == [] || pruning  = best'
  | otherwise = makeMoves restMoves board depth alpha' beta player best'
  where
    nextBoard = makeMove board player move
    value = negate $ alphaBeta' nextBoard (getNextPlayer player) (depth-1) (-beta) (-alpha)
    best' = if value > best then value else best
    alpha' = if best' > alpha then best' else alpha
    pruning = best' >= beta


getNextPlayer :: Player -> Player
getNextPlayer x = if x == White then Black else White

getActions :: Board -> Player -> [MoveHolder]
getActions board player = if length jumps' /= 0 then jumps' else moves'
  where
    moves' = map NormalMove $ getMoves board player
    jumps' = map JumpMove . filterJumps $ getJumps board player

filterJumps :: [Jump] -> [Jump]
filterJumps jumps'
  | length jumps' == 0 = jumps'
  | otherwise = result
  where
    max' = jumpSize . maximumBy (\a b -> compare (jumpSize a) (jumpSize b)) $ jumps'
    result = filter (\x -> jumpSize x == max') jumps'

getMoves :: Board -> Player -> [Move]
getMoves board player
  | player == White = moves $ getWhiteMovesList board
  | otherwise = moves $ getBlackMovesList board

getJumps :: Board -> Player -> [Jump]
getJumps board player
  | player == White = jumps $ getWhiteJumpsList board
  | otherwise = jumps $ getBlackJumpsList board

isGameEnded :: Board -> Bool
isGameEnded board = bp board == 0 || wp board == 0
