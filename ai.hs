{-# LANGUAGE BangPatterns #-}
module Ai where

import           Board
import           Data.Int
import           Data.IORef
import           Data.Word
import           Debug.Trace
import           Eval
import           Moves
import           System.Clock
import           Table

type Counter = IORef Int

data AlphaResult = AlphaResult Int MoveHolder | Timeout deriving(Eq, Show)


takeValue (AlphaResult value _) = value
takeMove (AlphaResult _ move) = move

getCurrentTime :: IO Int64
getCurrentTime = fmap sec (getTime Realtime)

maxDeep :: Int
maxDeep = 80

maxTime :: Int64
maxTime = 3*60

iterativeDeepening :: GameState -> TTableRef -> IO AlphaResult
iterativeDeepening gameState v = do
  time <- getCurrentTime
  counter <- newIORef 0
  !result <- iterativeDeepening' gameState v 6 0 time counter $ AlphaResult (-maxEval) None
  counter' <- readIORef counter
  trace ("Nodes visited: " ++ (show counter')) $ return result

iterativeDeepening' :: GameState -> TTableRef -> Int -> Int -> Int64 -> Counter -> AlphaResult -> IO AlphaResult
iterativeDeepening' gameState v depth firstGuess time counter previousResult = do
  !result <- mtdf gameState v firstGuess depth time counter
  trace ("Depth: " ++ (show depth) ++ " result:" ++ (show result)) $ if result == Timeout then return previousResult
  else if depth >= maxDeep then return result
  else iterativeDeepening' gameState v (depth + 2) (takeValue result) time counter result

mtdf :: GameState -> TTableRef -> Int -> Int -> Int64 -> Counter -> IO AlphaResult
mtdf gameState v first = mtdf' gameState v g upperBound lowerBound
  where
    g = AlphaResult first None
    upperBound = maxEval
    lowerBound = -maxEval

mtdf' :: GameState -> TTableRef -> AlphaResult -> Int -> Int -> Int -> Int64 -> Counter -> IO AlphaResult
mtdf' gameState v result@(AlphaResult g _) upperBound lowerBound depth time counter = do
  let beta = if lowerBound == g then g + 1 else g
  if lowerBound >= upperBound then return result
  else do
    !alphaResult <- alphaBeta' gameState depth (beta - 1) beta v time counter
    if alphaResult == Timeout then return Timeout
    else do
      let g' = takeValue alphaResult
      let upperBound' =  if g' < beta then g' else upperBound
      let lowerBound' = if g' >= beta then g' else lowerBound
      mtdf' gameState v alphaResult upperBound' lowerBound' depth time counter


alphaBeta' :: GameState -> Int -> Int -> Int ->  TTableRef -> Int64 -> Counter -> IO AlphaResult
alphaBeta' gameState@(GameState board player hash) depth alpha beta v time counter = do
  counter' <- readIORef counter

  currentTime <- if counter' `mod` 50000 == 0 then getCurrentTime else return time

  if currentTime - time > maxTime then return Timeout
  else do
    writeIORef counter (counter' + 1)
    ttEntry <- readTT hash v

    let isOldDepth = tDepth ttEntry < depth
    let alpha' = if ttEntry == TTNone || isOldDepth then alpha else max alpha $ if tFlag ttEntry == LowerBound then tValue ttEntry else alpha
    let beta' =  if ttEntry == TTNone || isOldDepth then beta else min beta $ if tFlag ttEntry == UpperBound then tValue ttEntry else beta
    let bestMoveForNode = if ttEntry /= TTNone then tMove ttEntry else None
    let tmpActions = getActions board player
    let actions = if bestMoveForNode /= None then bestMoveForNode:tmpActions else tmpActions

    if ttEntry /= TTNone && not isOldDepth && (tFlag ttEntry == Exact || alpha' >= beta') then return $ AlphaResult (tValue ttEntry) (tMove ttEntry)
    else if isGameEnded board then return $ AlphaResult (evaluate board player) None
    else if depth == 0 then return $ quiesceBoard gameState
    else if null actions then return $ AlphaResult (-maxEval) None
    else makeMoves actions gameState depth (AlphaResult (-maxEval - 1) None) alpha' hash alpha' beta' v time counter


makeMoves :: [MoveHolder] -> GameState -> Int -> AlphaResult -> Int -> Word64 -> Int -> Int -> TTableRef -> Int64 -> Counter -> IO AlphaResult
makeMoves (move:restMoves) gameState depth (AlphaResult best bestMove) alphaOrigin sourceHash alpha beta v time counter = do
  let nextState = doMove gameState move
  alphaResult <- alphaBeta' nextState (depth-1) (-beta) (-alpha) v time counter

  if alphaResult == Timeout then return alphaResult
  else do
    let value = negate . takeValue $ alphaResult
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
      return $ AlphaResult best' bestMove'
      )
    else makeMoves restMoves gameState depth (AlphaResult best' bestMove') alphaOrigin sourceHash alpha' beta v time counter

quiesceBoard :: GameState -> AlphaResult
quiesceBoard gameState@(GameState board player _)
  | null jumpMoves = AlphaResult (evaluate board player) None
  | otherwise = quiesceResult
  where
    jumpMoves = map JumpMove . filterJumps $ getJumps board player
    quiesceResult = quiesceMoves jumpMoves gameState $ AlphaResult (-maxEval -1) None

quiesceMoves:: [MoveHolder] -> GameState -> AlphaResult -> AlphaResult
quiesceMoves (jump:restJumps) gameState (AlphaResult best _)
  | null restJumps = AlphaResult best' None
  | otherwise = quiesceMoves restJumps gameState $ AlphaResult best' None
  where
    newState = doMove gameState jump
    result = quiesceBoard newState
    value = negate . takeValue $ result
    best' = max value best
