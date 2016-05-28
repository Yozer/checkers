{-# LANGUAGE BangPatterns #-}
module Ai where

import           Board
import           Control.Parallel.Strategies
import           Eval
import           Moves


type AlphaResult = (Int, MoveHolder)


maxDeep :: Int
maxDeep = 15

alphaBeta :: Board -> Player -> AlphaResult
alphaBeta board player = alphaBeta' board player maxDeep (-maxEval - 1) maxEval



alphaBeta' :: Board -> Player -> Int -> Int -> Int -> AlphaResult
alphaBeta' board player depth = makeMoves actions board depth player (-maxEval - 1, None)
  where
    actions = getActions board player



makeMoves :: [MoveHolder] -> Board -> Int -> Player -> AlphaResult -> Int -> Int -> AlphaResult
makeMoves (m:restMoves) board depth player (best, move) alpha beta
  | null restMoves || pruning = (best', move')
  | otherwise = makeMoves restMoves board depth player (best', move') alpha' beta
  where
    nextBoard = doMove board player m
    value = negate $ alphaBeta'' nextBoard (getNextPlayer player) (depth-1) (-beta) (-alpha)
    (best', move') = if value > best then (value, m) else (best, move)
    alpha' = if value > alpha then value else alpha
    pruning = alpha' >= beta

makeMoves [] _ _ _ best _ _= best


alphaBeta'' :: Board -> Player -> Int -> Int -> Int -> Int
alphaBeta'' board player depth alpha beta
  | depth == 0 || isGameEnded board = evaluate board player (maxDeep - depth) alpha beta
  | null actions = -maxEval
  | otherwise = makeMoves' actions board depth player (-maxEval) alpha beta
  where
    actions = getActions board player



makeMoves' :: [MoveHolder] -> Board -> Int -> Player -> Int -> Int -> Int -> Int
makeMoves' (move:restMoves) board depth player best alpha beta
  | null restMoves || pruning = best'
  | otherwise = makeMoves' restMoves board depth player best' alpha' beta
  where
    nextBoard = doMove board player move
    value = negate $ alphaBeta'' nextBoard (getNextPlayer player) (depth-1) (-beta) (-alpha)
    best' = if value > best then value else best
    alpha' = if value > alpha then value else alpha
    pruning = alpha' >= beta

makeMoves' [] _ _ _ best _ _= best


getNextPlayer :: Player -> Player
getNextPlayer x = if x == White then Black else White
