module Main where

import Board
import Moves
import Ai
import Masks
import Eval
import Data.Word

main :: IO ()
main = loop initialBoard

printMove :: MoveHolder -> IO ()
printMove (JumpMove x) = print path
  where
    path = map rfield $ jumpPath x
printMove (NormalMove x) = putStrLn $ show (rfield $ src x) ++ " do " ++ show(rfield $ dst x)
printMove _ = undefined 


moveMath from to (NormalMove x) = (rfield $ src x) == from && (rfield $ dst x) == to
moveMath from to (JumpMove x) = head path == from && last path == to
  where
    path = map rfield $ jumpPath x

loop :: Board -> IO ()
loop board = do

  let (value, move) = alphaBeta board
  let board' = makeMove board White move
  print (value, move)

  putStrLn "Po alpha beta:"
  printBoard board'


  -- from <- getLine
  -- to <- getLine

  -- let from' = read from::Int
  -- let to' = read to::Int

  -- let possibleActions = getActions board' Black
  -- let blackMove = filter (\x -> moveMath from' to' x) possibleActions

  -- let board'' = makeMove board' Black $ head blackMove
  -- putStrLn "Po graczu:"
  -- printBoard board''
  -- loop board''

