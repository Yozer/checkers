{-# LANGUAGE BangPatterns #-}
module Ai where

import           Board
import           Control.Parallel.Strategies
import           Data.Word
import           Debug.Trace
import           Eval
import           Moves
import           Table

type AlphaResult = (Int, MoveHolder)


maxDeep :: Int
maxDeep = 15

iterativeDeepening :: GameState -> TTableRef -> IO AlphaResult
iterativeDeepening gameState v = iterativeDeepening' gameState v 1 0

iterativeDeepening' :: GameState -> TTableRef -> Int -> Int -> IO AlphaResult
iterativeDeepening' gameState v depth firstGuess = do
  result@(firstGuess', _) <- mtdf gameState v firstGuess depth
  if depth >= maxDeep then return result
  else iterativeDeepening' gameState v (depth + 2) firstGuess'

mtdf :: GameState -> TTableRef -> Int -> Int -> IO AlphaResult
mtdf gameState v first depth = mtdf' gameState v g upperBound lowerBound depth
  where
    g = (first, None)
    upperBound = maxEval
    lowerBound = -maxEval

mtdf' :: GameState -> TTableRef -> AlphaResult -> Int -> Int -> Int -> IO AlphaResult
mtdf' gameState v result@(g, _) upperBound lowerBound depth = do
  let beta = if lowerBound == g then g + 1 else g
  if lowerBound >= upperBound then return result
  else do 
    result'@(g', _) <-alphaBeta' gameState depth (beta - 1) beta v
    let upperBound' =  if g' < beta then g' else upperBound
    let lowerBound' = if g' >= beta then g' else lowerBound
    mtdf' gameState v result' upperBound' lowerBound' depth


alphaBeta' :: GameState -> Int -> Int -> Int ->  TTableRef -> IO AlphaResult
alphaBeta' gameState@(GameState board player hash) depth alpha beta v = do
  ttEntry <- readTT hash v

  let isOldDepth = tDepth ttEntry < depth
  let alpha' = if ttEntry == TTNone || isOldDepth then alpha else max alpha $ if tFlag ttEntry == LowerBound then tValue ttEntry else alpha
  let beta' =  if ttEntry == TTNone || isOldDepth then beta else min beta $ if tFlag ttEntry == UpperBound then tValue ttEntry else beta
  let bestMoveForNode = if ttEntry /= TTNone then tMove ttEntry else None
  let tmpActions = getActions board player
  let actions = if bestMoveForNode /= None then bestMoveForNode:tmpActions else tmpActions

  if ttEntry /= TTNone && (not isOldDepth) && (tFlag ttEntry == Exact || alpha' >= beta') then return $ (tValue ttEntry, tMove ttEntry)
  else if depth == 0 || isGameEnded board then return $ (evaluate board player alpha' beta', None)
  else if null actions then return $ (-maxEval, None)
  else makeMoves actions gameState depth (-maxEval, None) alpha' hash alpha' beta' v


makeMoves :: [MoveHolder] -> GameState -> Int -> AlphaResult -> Int -> Word64 -> Int -> Int -> TTableRef -> IO AlphaResult
makeMoves (move:restMoves) gameState depth (best, bestMove) alphaOrigin sourceHash alpha beta v = do
  let nextState = doMove gameState move
  (tmpValue, _) <- alphaBeta' nextState (depth-1) (-beta) (-alpha) v
  let value = negate tmpValue
  let bestMove' = if value > best then move else bestMove
  let best' = max value best
  let alpha' = max alpha value
  let pruning = alpha' >= beta

  let flag = if best' <= alphaOrigin then UpperBound
             else if best' >= beta then LowerBound
             else Exact
  let tt = TTEntry {tValue = best', tFlag = flag, tDepth = depth, tHash = sourceHash, tMove = bestMove'}

  if null restMoves || pruning then (do
    writeTT sourceHash tt v
    return (best', bestMove')
    )
  else makeMoves restMoves gameState depth (best', bestMove') alphaOrigin sourceHash alpha' beta v
