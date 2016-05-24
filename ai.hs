module Ai where

import           Board
import           Control.Parallel.Strategies
import           Eval
import           Moves


type AlphaResult = (Float, MoveHolder)

maxDeep :: Int
maxDeep = 8

alphaBeta :: Board -> AlphaResult
alphaBeta board = maximum $  parMap rpar (\x -> (alphaBeta' (doMove board White x) Black maxDeep (maxEval) (-maxEval), x)) actions
  where
    actions = getActions board White


alphaBeta' :: Board -> Player -> Int -> Float -> Float -> Float
alphaBeta' board player depth alpha beta
  | depth == 0 || isGameEnded board || null actions = evaluate board player
  | otherwise = makeMoves actions board depth alpha beta player (-maxEval - 1)
  where
    actions = getActions board player


makeMoves :: [MoveHolder] -> Board -> Int -> Float -> Float -> Player -> Float -> Float
makeMoves (move:restMoves) board depth alpha beta player best
  | restMoves == [] || pruning  = best'
  | otherwise = makeMoves restMoves board depth alpha' beta player best'
  where
    nextBoard = doMove board player move
    value = negate $ alphaBeta' nextBoard (getNextPlayer player) (depth-1) (-beta) (-alpha)
    best' = if value > best then value else best
    alpha' = if best' > alpha then best' else alpha
    pruning = best' >= beta

makeMoves [] _ _ _ _ _ best = best

getNextPlayer :: Player -> Player
getNextPlayer x = if x == White then Black else White
