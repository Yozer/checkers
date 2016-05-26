module Main where

import           Ai
import           Board
import           Masks
import           Moves
import Data.List.Split
import Data.List


main :: IO ()
main = loop Board {wp=mergeBoardFields [6,12], bp=mergeBoardFields [14,29,31],k=0}

printMove :: MoveHolder -> IO ()
printMove (JumpMove x) = printPath x "x"
printMove (NormalMove x) = printPath x "-"
printMove _ = undefined

printPath :: Path -> String -> IO()
printPath path deli = putStrLn . intercalate deli . map show $ map rfield path


matchMove :: [MoveHolder] -> Int -> Int -> MoveHolder
matchMove actions from to = head . filter (isMoveMatching from to) $ actions

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

loop :: Board -> IO ()
loop board = do

  let (value, move) = alphaBeta board me
  let board' = doMove board me move

  putStrLn $ "After computer: " ++ (show value)
  printBoard board'
  printMove move

  let possibleActions = getActions board' opponent
  putStrLn "Possible moves:"
  mapM_ printMove possibleActions

  [from, to] <- splitOn " " <$> getLine

  let from' = read from::Int
  let to' = read to::Int

  let blackMove = matchMove possibleActions from' to'

  let board'' = doMove board' opponent blackMove
  putStrLn "After player:"
  printBoard board''
  loop board''

