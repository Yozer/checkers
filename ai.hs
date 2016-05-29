{-# LANGUAGE BangPatterns #-}
module Ai where

import           Board
import           Control.Parallel.Strategies
import           Eval
import           Moves
import Table
import Data.Word
import Debug.Trace

type AlphaResult = (Int, MoveHolder)


maxDeep :: Int
maxDeep = 18

iterativeDeepening :: GameState -> TTableRef -> IO AlphaResult
iterativeDeepening gameState v = iterativeDeepening' gameState v 1 0

iterativeDeepening' :: GameState -> TTableRef -> Int -> Int -> IO AlphaResult
iterativeDeepening' gameState v depth firstGuess = do
  result@(firstGuess', _) <- trace (show depth) $ mtdf gameState v firstGuess depth
  if depth == maxDeep then return result
  else iterativeDeepening' gameState v (depth + 1) firstGuess' 

mtdf :: GameState -> TTableRef -> Int -> Int -> IO AlphaResult
mtdf gameState v first depth = mtdf' gameState v g upperBound lowerBound depth
  where
    g = (first, None)
    upperBound = maxEval
    lowerBound = -maxEval

mtdf' :: GameState -> TTableRef -> AlphaResult -> Int -> Int -> Int -> IO AlphaResult
mtdf' gameState v result@(g, _) upperBound lowerBound depth = do
  let beta = if lowerBound == g then g + 1 else g
  result'@(g', _) <- alphaBeta' gameState depth v (beta - 1) beta
  let upperBound' = if g' < beta then g' else upperBound
  let lowerBound' = if g' >= beta then g' else lowerBound

  if lowerBound >= upperBound then return result
  else mtdf' gameState v result' upperBound' lowerBound' depth


--alphaBeta :: GameState -> TTableRef -> IO AlphaResult
--alphaBeta gameState v = alphaBeta' gameState maxDeep v (-maxEval - 1) maxEval



alphaBeta' :: GameState -> Int -> TTableRef -> Int -> Int -> IO AlphaResult
alphaBeta' (GameState board player hash) depth v = makeMoves actions (GameState board player hash) depth v (-maxEval - 1, None)
  where
    actions = getActions board player



makeMoves :: [MoveHolder] -> GameState -> Int -> TTableRef -> AlphaResult -> Int -> Int -> IO AlphaResult
makeMoves (m:restMoves) (GameState board player hash) depth v (best, move) alpha beta = do
  let nextState = doMove (GameState board player hash) m
  res <- alphaBeta'' nextState (depth-1) (-beta) (-alpha) v
  let value = negate res
  let (best', move') = if value > best then (value, m) else (best, move)
  let alpha' = max value alpha
  let pruning = alpha' >= beta

  if null restMoves || pruning then return (best', move')
  else makeMoves restMoves (GameState board player hash) depth v (best', move') alpha' beta


alphaBeta'' :: GameState -> Int -> Int -> Int ->  TTableRef -> IO Int
alphaBeta'' (GameState board player hash) depth alpha beta v = do
  ttEntry <- readTT hash depth v

  let alpha' = if ttEntry == TTNone then alpha else max alpha $ if tFlag ttEntry == LowerBound then tValue ttEntry else alpha
  let beta' =  if ttEntry == TTNone then beta else min beta $ if tFlag ttEntry == UpperBound then tValue ttEntry else beta

  if ttEntry /= TTNone && (tFlag ttEntry == Exact || alpha' >= beta') then return $ tValue ttEntry
  else if depth == 0 || isGameEnded board then return $ evaluate board player (maxDeep - depth) alpha' beta'
  else if null actions then return $ -maxEval
  else makeMoves' actions (GameState board player hash) depth (-maxEval) alpha' hash alpha' beta' v
  where
    actions = getActions board player
    



makeMoves' :: [MoveHolder] -> GameState -> Int -> Int -> Int -> Word64 -> Int -> Int -> TTableRef -> IO Int
makeMoves' (move:restMoves) (GameState board player hash) depth best alphaOrigin sourceHash alpha beta v = do
  let nextState = doMove (GameState board player hash) move
  res <- alphaBeta'' nextState (depth-1) (-beta) (-alpha) v
  let value = negate res
  let best' = max value best
  let alpha' = max alpha value
  let pruning = alpha' >= beta

  let flag = if best' <= alphaOrigin then UpperBound
             else if best' >= beta then LowerBound
             else Exact
  let tt = TTEntry {tValue = best', tFlag = flag, tDepth = depth, tHash = sourceHash}

  if null restMoves || pruning then (do 
    writeTT sourceHash tt v
    return best'
    ) 
  else makeMoves' restMoves (GameState board player hash) depth best' alphaOrigin sourceHash alpha' beta v