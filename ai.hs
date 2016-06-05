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
import           Control.Monad

type Counter = IORef Int


data AlphaResult = AlphaResult Int MoveHolder | Timeout deriving(Eq, Show)

takeValue (AlphaResult value _) = value
takeMove (AlphaResult _ move) = move

maxDepth :: Int
maxDepth = 52
maxTime :: Int64
maxTime = 5


getCurrentTime :: Clock -> IO Int64
getCurrentTime clock = getTime clock >>= return . sec

isTimeElapsed :: Int64 -> Int64 -> Bool
isTimeElapsed time currentTime = (currentTime - time) > maxTime

iterativeDeepening :: GameState -> TTableRef -> IO AlphaResult
iterativeDeepening gameState v = do
  let clock = Realtime
  counter <- newIORef 0
  time <- getCurrentTime clock
  !res <- iterativeDeepening' gameState v 10 0 time clock counter

  --if res =

  counter' <- readIORef counter
  print counter'
  return res

iterativeDeepening' :: GameState -> TTableRef -> Int -> Int -> Int64 -> Clock -> Counter -> IO AlphaResult
iterativeDeepening' gameState v depth firstGuess time clock counter = do
  !result <- mtdf gameState v firstGuess depth time clock counter
  trace ("depth: " ++ (show depth) ++ ":" ++ (show result) ++ ":" ++ (show firstGuess)) $ if depth >= maxDepth || result == Timeout then return result
  else do
    !recurResult <- iterativeDeepening' gameState v (depth + 2) (takeValue result) time clock counter
    return $ if recurResult == Timeout then result else recurResult

mtdf :: GameState -> TTableRef -> Int -> Int -> Int64 -> Clock -> Counter -> IO AlphaResult
mtdf gameState v first depth time clock counter = do
  let g = AlphaResult first None
  let upperBound = maxEval
  let lowerBound = -maxEval
  result <- mtdf' gameState v g upperBound lowerBound depth time clock counter
  return $ if result == Timeout || takeMove result == None then Timeout else result


mtdf' :: GameState -> TTableRef -> AlphaResult -> Int -> Int -> Int -> Int64 -> Clock -> Counter -> IO AlphaResult
mtdf' gameState v result@(AlphaResult g _) upperBound lowerBound depth time clock counter = do
  let beta = if lowerBound == g then g + 1 else g
  currentTime <- getCurrentTime clock
  if lowerBound >= upperBound then return result
  else if isTimeElapsed time currentTime then return Timeout
  else do
    !result' <- alphaBeta' gameState depth (beta - 1) beta v time clock counter
    if result' == Timeout then return Timeout
    else do
      let g' = takeValue result'
      let upperBound' =  if g' < beta then g' else upperBound
      let lowerBound' = if g' >= beta then g' else lowerBound
      mtdf' gameState v result' upperBound' lowerBound' depth time clock counter

mtdf' _ _ Timeout _ _ _ _ _ _ = return Timeout



alphaBeta' :: GameState -> Int -> Int -> Int ->  TTableRef -> Int64 -> Clock -> Counter -> IO AlphaResult
alphaBeta' gameState@(GameState board player hash) depth alpha beta v time clock counter = do
  counter' <- readIORef counter
  writeIORef counter (counter' + 1)
  currentTime <- if counter' `mod` 50000 == 0 then getCurrentTime clock else return time
  if  isTimeElapsed time currentTime then return Timeout
  else do
    ttEntry <- readTT hash v

    let isOldDepth = tDepth ttEntry < depth
    let alpha' = if ttEntry == TTNone || isOldDepth then alpha else max alpha $ if tFlag ttEntry == LowerBound then tValue ttEntry else alpha
    let beta' =  if ttEntry == TTNone || isOldDepth then beta else min beta $ if tFlag ttEntry == UpperBound then tValue ttEntry else beta
    let bestMoveForNode = if ttEntry /= TTNone then tMove ttEntry else None
    let tmpActions = getActions board player
    let actions = if bestMoveForNode /= None then bestMoveForNode:tmpActions else tmpActions

    -- when (counter' == 0) $ do
    --   print actions
    --   if ttEntry /= TTNone && tMove ttEntry /= None && (not isOldDepth) && (tFlag ttEntry == Exact || alpha' >= beta') then print "test1"
    --   else if depth == 0 || isGameEnded board then print "test2"
    --   else if null actions then print "test3"
    --   else print "test4"

    if ttEntry /= TTNone && tMove ttEntry /= None && (not isOldDepth) && (tFlag ttEntry == Exact || alpha' >= beta') then return $ AlphaResult (tValue ttEntry) (tMove ttEntry)
    else if depth == 0 || isGameEnded board then return $ AlphaResult (evaluate board player alpha' beta') None
    else if null actions then return $ AlphaResult (-maxEval) None
    else makeMoves actions gameState depth (AlphaResult (-maxEval) (head actions)) alpha' hash alpha' beta' v time clock counter


makeMoves :: [MoveHolder] -> GameState -> Int -> AlphaResult -> Int -> Word64 -> Int -> Int -> TTableRef -> Int64 -> Clock -> Counter -> IO AlphaResult
makeMoves (move:restMoves) gameState depth (AlphaResult best bestMove) alphaOrigin sourceHash alpha beta v time clock counter = do
  let nextState = doMove gameState move
  result <- alphaBeta' nextState (depth-1) (-beta) (-alpha) v time clock counter
  if result == Timeout then return Timeout
  else do
    let value = negate . takeValue $ result
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
    else makeMoves restMoves gameState depth (AlphaResult best' bestMove') alphaOrigin sourceHash alpha' beta v time clock counter

