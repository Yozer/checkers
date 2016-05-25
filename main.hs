module Main where

import           Ai
import           Board
import           Masks
import           Moves
import Data.List.Split
import Data.List


main :: IO ()
main = loop  initialBoard

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

loop :: Board -> IO ()
loop board = do

  let (value, move) = alphabeta 9 $ AlphaInfo board White
  let board' = doMove board White move

  putStrLn $ "After computer: " ++ (show value)
  printBoard board'
  printMove move

  let possibleActions = getActions board' Black
  putStrLn "Possible moves:"
  mapM_ printMove possibleActions

  [from, to] <- splitOn " " <$> getLine

  let from' = read from::Int
  let to' = read to::Int

  let blackMove = matchMove possibleActions from' to'

  let board'' = doMove board' Black blackMove
  putStrLn "After player:"
  printBoard board''
  loop board''

