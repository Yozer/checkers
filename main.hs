module Main where

import           Ai
import           Board
import           Masks
import           Moves
import Data.List.Split
import Data.List
import Table


main :: IO ()
main = do
  table <- allocate
  (if me == White then loopWhite else loopBlack) (initialGameState me) table

printMove :: MoveHolder -> IO ()
printMove (JumpMove x) = printPath x "x"
printMove (NormalMove x) = printPath x "-"
printMove _ = undefined

printPath :: Path -> String -> IO()
printPath path deli = putStrLn . intercalate deli . map show $ map rfield path


matchMove :: [MoveHolder] -> Int -> Int -> MoveHolder
matchMove actions from to
  | null matchedActions = None
  | otherwise = head matchedActions
  where
    matchedActions = filter (isMoveMatching from to) $ actions

isMoveMatching :: Int -> Int -> MoveHolder -> Bool
isMoveMatching from to (NormalMove x) = isMoveMatching' from to x
isMoveMatching from to (JumpMove x) = isMoveMatching' from to x
isMoveMatching _ _ _ = False
isMoveMatching' :: Int -> Int -> Path -> Bool
isMoveMatching' from to path = head path' == (fromIntegral from) && last path' == (fromIntegral to)
  where
    path' = map rfield path


me = White
opponent = Black

loopWhite :: GameState -> TTableRef -> IO ()
loopWhite gameState table = do

  (value, move) <- iterativeDeepening gameState table
  let GameState board player hash = doMove gameState move

  putStrLn $ "After computer: " ++ (show value)
  printBoard board
  printMove move

  let possibleActions = getActions board opponent
  putStrLn "Possible moves:"
  mapM_ printMove possibleActions

  [from, to] <- splitOn " " <$> getLine

  let from' = read from::Int
  let to' = read to::Int

  let blackMove = matchMove possibleActions from' to'

  let GameState board' player' hash' = doMove (GameState board player hash) blackMove
  putStrLn "After player:"
  printBoard board'
  loopWhite (GameState board' player' hash') table



loopBlack :: GameState -> TTableRef -> IO ()
loopBlack (GameState board player hash) table = do

  let possibleActions = getActions board opponent
  putStrLn "Possible moves:"
  mapM_ printMove possibleActions

  [from, to] <- splitOn " " <$> getLine

  let from' = read from::Int
  let to' = read to::Int

  let blackMove = matchMove possibleActions from' to'
  let GameState board' player' hash' = doMove (GameState board player hash) blackMove

  putStrLn "After player:"
  printBoard board'

  (value, move) <- iterativeDeepening (GameState board' player' hash') table
  let GameState board'' player'' hash'' = doMove (GameState board' player' hash') move

  putStrLn $ "After computer: " ++ (show value)
  printBoard board''
  printMove move
  loopBlack (GameState board'' player'' hash'') table