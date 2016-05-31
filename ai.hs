{-# LANGUAGE BangPatterns #-}
module Ai where

import           Board
import           Control.Parallel.Strategies
import           Data.Word
import           Debug.Trace
import           Eval
import           Moves
import           Table
import Data.Time.Clock.POSIX

data AlphaResult = AlphaResult Int MoveHolder | Timeout deriving(Eq, Show)

takeValue (AlphaResult value _) = value
takeMove (AlphaResult _ move) = move

maxDeep :: Int
maxDeep = 52
maxTime :: Int
maxTime = 2*60-3

-- search board = do
  
--   gen <- R.create
--   t <- search' time (getRoot board White) gen
--   -- let trials = 200000
--   -- t <- withSystemRandom . asGenST $ \gen -> run gen trials -- tworzymy randoma i odpalamy drzwo
--   --print $ allChildren t
--   return . move . maximumBy (\a b -> compare (value a / (realToFrac $ visits a)) (value b / (realToFrac $ visits b))) $ allChildren t

-- search' :: Int -> FullUctNode -> R.Gen GHC.Prim.RealWorld -> IO FullUctNode
-- search' time !t gen = do
--   currentTime <-  round `fmap` getPOSIXTime 
--   !newTree <- selectAction gen t
--   if currentTime >= (time + maxTime) then return newTree
--   else search' time newTree gen

getTime :: IO Int
getTime = round `fmap` getPOSIXTime 

isTimeElapsed :: Int -> Int -> Bool
isTimeElapsed time currentTime = (currentTime - time) > maxTime

iterativeDeepening :: GameState -> TTableRef -> IO AlphaResult
iterativeDeepening gameState v = do
  time <- getTime
  iterativeDeepening' gameState v 6 0 time

iterativeDeepening' :: GameState -> TTableRef -> Int -> Int -> Int -> IO AlphaResult
iterativeDeepening' gameState v depth firstGuess time = do
  result <- mtdf gameState v firstGuess depth time
  if depth >= maxDeep then return result
  else if result == Timeout then return Timeout
  else do
    recurResult <- iterativeDeepening' gameState v (depth + 2) (takeValue result) time
    return $ if recurResult == Timeout then result else recurResult

mtdf :: GameState -> TTableRef -> Int -> Int -> Int -> IO AlphaResult
mtdf gameState v first = mtdf' gameState v g upperBound lowerBound
  where
    g = AlphaResult first None
    upperBound = maxEval
    lowerBound = -maxEval

mtdf' :: GameState -> TTableRef -> AlphaResult -> Int -> Int -> Int -> Int -> IO AlphaResult
mtdf' gameState v result@(AlphaResult g _) upperBound lowerBound depth time = do
  let beta = if lowerBound == g then g + 1 else g
  currentTime <- getTime
  if lowerBound >= upperBound then return result
  else if isTimeElapsed time currentTime then return Timeout
  else do 
    result' <-alphaBeta' gameState depth (beta - 1) beta v time
    if result' == Timeout then return Timeout
    else do
      let g' = takeValue result'
      let upperBound' =  if g' < beta then g' else upperBound
      let lowerBound' = if g' >= beta then g' else lowerBound
      mtdf' gameState v result' upperBound' lowerBound' depth time

mtdf' _ _ Timeout _ _ _ _ = return Timeout

alphaBeta' :: GameState -> Int -> Int -> Int ->  TTableRef -> Int -> IO AlphaResult
alphaBeta' gameState@(GameState board player hash) depth alpha beta v time = do
  currentTime <- getTime
  if isTimeElapsed time currentTime then return Timeout
  else do
    ttEntry <- readTT hash v

    let isOldDepth = tDepth ttEntry < depth
    let alpha' = if ttEntry == TTNone || isOldDepth then alpha else max alpha $ if tFlag ttEntry == LowerBound then tValue ttEntry else alpha
    let beta' =  if ttEntry == TTNone || isOldDepth then beta else min beta $ if tFlag ttEntry == UpperBound then tValue ttEntry else beta
    let bestMoveForNode = if ttEntry /= TTNone then tMove ttEntry else None
    let tmpActions = getActions board player
    let actions = if bestMoveForNode /= None then bestMoveForNode:tmpActions else tmpActions

    if ttEntry /= TTNone && (not isOldDepth) && (tFlag ttEntry == Exact || alpha' >= beta') then return $ AlphaResult (tValue ttEntry) (tMove ttEntry)
    else if depth == 0 || isGameEnded board then return $ AlphaResult (evaluate board player alpha' beta') None
    else if null actions then return $ AlphaResult (-maxEval) None
    else makeMoves actions gameState depth (AlphaResult (-maxEval) None) alpha' hash alpha' beta' v time


makeMoves :: [MoveHolder] -> GameState -> Int -> AlphaResult -> Int -> Word64 -> Int -> Int -> TTableRef -> Int -> IO AlphaResult
makeMoves (move:restMoves) gameState depth (AlphaResult best bestMove) alphaOrigin sourceHash alpha beta v time = do
  let nextState = doMove gameState move
  result <- alphaBeta' nextState (depth-1) (-beta) (-alpha) v time
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
    else makeMoves restMoves gameState depth (AlphaResult best' bestMove') alphaOrigin sourceHash alpha' beta v time
