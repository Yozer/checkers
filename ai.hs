module Ai where

import Board
import Eval
import Moves
import Data.Ord
import Data.List

maxEval :: Float
maxEval = 200

alphaBeta :: Board -> Player -> Int -> Float -> Float -> Float
alphaBeta board player depth alpha beta
  | depth == 0 || isGameEnded board || (jumpsCount jumps' == 0 && movesCount moves' == 0) = evaluate board player
  | otherwise = if jumpsCount jumps' /= 0 
                then makeJumps (jumps jumps') board depth alpha beta player (-maxEval - 1) 
                else makeMoves (moves moves') board depth alpha beta player (-maxEval - 1)
  where
    moves' = getMoves board player
    jumps' = filterJumps $ getJumps board player




makeMoves :: MoveList -> Board -> Int -> Float -> Float -> Player -> Float -> Float
makeMoves (move:restMoves) board depth alpha beta player best
  | restMoves == [] = best
  | pruning = best
  | otherwise = makeMoves restMoves board depth alpha' beta player best'
  where
    nextBoard = doMove board player move
    
    value = negate $ alphaBeta nextBoard (getNextPlayer player) (depth-1) (-beta) (-alpha)
    best' = if value > best then value else best
    alpha' = if best' > alpha then best' else alpha
    pruning = best' >= beta

makeJumps :: JumpList -> Board -> Int -> Float -> Float -> Player -> Float -> Float
makeJumps (move:restMoves) board depth alpha beta player best
  | restMoves == [] = best
  | pruning = best
  | otherwise = makeJumps restMoves board depth alpha' beta player best'
  where
    nextBoard = doJump board player move
    value = negate $ alphaBeta nextBoard (getNextPlayer player) (depth-1) (-beta) (-alpha)
    best' = if value > best then value else best
    alpha' = if best' > alpha then best' else alpha
    pruning = best' >= beta


getNextPlayer :: Player -> Player
getNextPlayer x = if x == White then Black else White

filterJumps :: JumpData -> JumpData
filterJumps jumps'
  | jumpsCount jumps' == 0 = jumps'
  | otherwise = result
  where
    max' = jumpSize . maximumBy (\a b -> compare (jumpSize a) (jumpSize b)) $ jumps jumps'
    maxJumps = filter (\x -> jumpSize x == max') $ jumps jumps'
    result = JumpData {jumps = maxJumps, jumpsCount = length maxJumps}

getMoves :: Board -> Player -> MoveData
getMoves board player
  | player == White = getWhiteMovesList board
  | otherwise = getBlackMovesList board

getJumps :: Board -> Player -> JumpData
getJumps board player
  | player == White = getWhiteJumpsList board
  | otherwise = getBlackJumpsList board

isGameEnded :: Board -> Bool
isGameEnded board = bp board == 0 || wp board == 0