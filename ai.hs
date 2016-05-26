module Ai where

import           Board
import           Control.Parallel.Strategies
import           Eval
import           Moves


type AlphaResult = (Float, MoveHolder)

maxDeep :: Int
maxDeep = 15

alphaBeta :: Board -> Player -> AlphaResult
alphaBeta board player = minimum $  parMap rpar (\x -> (alphaBeta' (doMove board player x) (getNextPlayer player) maxDeep (-maxEval) maxEval, x)) actions
  where
    actions = getActions board player


alphaBeta' :: Board -> Player -> Int -> Float -> Float -> Float
alphaBeta' board player depth alpha beta
  | depth == 0 || isGameEnded board = evaluate board player
  | null actions = -maxEval
  | otherwise = makeMoves actions board depth player (-maxEval) alpha beta
  where
    actions = getActions board player


makeMoves :: [MoveHolder] -> Board -> Int -> Player -> Float -> Float -> Float -> Float
makeMoves (move:restMoves) board depth player best alpha beta
  | null restMoves || pruning = best'
  | otherwise = makeMoves restMoves board depth player best' alpha' beta
  where
    nextBoard = doMove board player move
    value = negate $ alphaBeta' nextBoard (getNextPlayer player) (depth-1) (-beta) (-alpha)
    best' = if value > best then value else best
    alpha' = if value > alpha then value else alpha
    pruning = alpha' >= beta

makeMoves [] _ _ _ best _ _= best

getNextPlayer :: Player -> Player
getNextPlayer x = if x == White then Black else White
