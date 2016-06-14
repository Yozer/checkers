{-# LANGUAGE BangPatterns #-}

module Main where

import           Ai
import           Board
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Masks
import           Moves
import           System.Environment
import           System.IO
import           Table
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Number
import qualified Data.Vector.Unboxed as V
import Eval
import Debug.Trace

printMove :: MoveHolder -> String
printMove (JumpMove x) = printPath x "x"
printMove (NormalMove x) = printPath x "-"
printMove _ = "None"


printPath :: Path -> String -> String
--printPath path deli = intercalate deli . map (show . reverseBoardIndexing . rfield) $ path
printPath path deli = intercalate deli . map (show  . rfield) $ path

data PDN =   Move (Int,Int) -- pozycja startowa i koncowa
           | Kill [Int]  -- pozycja startowa to glowa, pozniej kolejne pozycje
           deriving(Show)


matchMove :: [MoveHolder] -> PDN -> MoveHolder
matchMove actions (Move (from, to)) = matchMove' actions [from, to]
matchMove actions (Kill x) = matchMove' actions x

matchMove' :: [MoveHolder] -> [Int] -> MoveHolder
matchMove' actions path
  | null actions = None
  | null matchedActions = Invalid
  | otherwise = head matchedActions
  where
    matchedActions = filter (isMoveMatching path) actions

isMoveMatching :: [Int] -> MoveHolder -> Bool
-- isMoveMatching path (NormalMove x) = (getBoardFields . map reverseBoardIndexing $ path) == x
-- isMoveMatching path (JumpMove x) = (getBoardFields . map  reverseBoardIndexing $ path) == x
isMoveMatching path (NormalMove x) = (getBoardFields path) == x
isMoveMatching path (JumpMove x) = (getBoardFields  path) == x
isMoveMatching _ _ = False




parsePos :: Parser Int
parsePos = do
            x <- int
            if x<1 || x>32 then
              unexpected "Tylko liczby od 1-32"
            else
              return x

parseMove = do
            x1 <- parsePos
            char '-'
            x2 <- parsePos
            eof
            return $ Move (x1,x2)

parseKill = do
            x1 <- sepBy parsePos (char 'x')
            eof
            case x1 of
              [] -> unexpected "cos musi byc"
              _  -> return $ Kill x1

parsePDN =  try parseMove <|> try parseKill


readMove :: [MoveHolder] -> IO MoveHolder
readMove actions = do
  !line <- getLine
  return $ case parse parsePDN "sPDN err" line of
                  Right m -> matchMove actions m
                  Left _ -> None

--txtFile = "program1.txt"

loopGame :: GameState -> TTableRef -> Player -> IO ()
loopGame state@(GameState board player _) table me = do
  let possibleActions = getActions board player

  AlphaResult _ !move <- if player == me then iterativeDeepening state table
                             else (do
                                      -- hPutStrLn stderr "Possible actions"
                                      -- when (me == White) $ mapM_ (hPutStrLn stderr . printMove) possibleActions
                                      !x <- readMove possibleActions
                                      return $ AlphaResult 0 x)

  if move == Invalid then trace "Invalid move" $ loopGame state table me
  else do
    when (move /= None) (do
      when (player == me) . putStrLn . printMove $ move
      let newState@(GameState board' _ _) = doMove state move
      hPutStrLn stderr . (((show player) ++ ": ")++) . printMove $ move
      hPutStrLn stderr . printBoard $ board'
      loopGame newState table me)

    when (move == None) $ if not . null $ possibleActions then putStrLn "Timeout" else putStrLn (show player ++ " lost")
  --when (move == None) $ hPutStrLn stderr $ show player ++ ": None move!!!!"



main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  table <- allocate
  args <- getArgs
  --let weights = V.fromList . read $ args !! 1
  --print weights
  -- progName <- getProgName
  --mapM_ putStrLn args
  -- putStrLn progName
  --let args = ["w"] -- do zakomentowania w programmie
  case listToMaybe args of
    Just "b" -> loopGame initialGameState table Black
    Just "w" -> loopGame initialGameState table White
    Nothing -> loopGame initialGameState table White
