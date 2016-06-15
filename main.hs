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
-- printPath path deli = intercalate deli . map (show . reverseBoardIndexing . rfield) $ path
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
  | null matchedActions = trace ("Invalid: " ++ (show path) ) $Invalid
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

  if move == Invalid then loopGame state table me
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
  let testBoards = [initialBoard,
                    Board {wp = 27021735203700736, bp = 8796663447552, k = 536870912},
                    Board {wp = 35218900255232, bp = 36028797018963968, k = 33685504},
                    Board {wp = 343597383680, bp = 9051352190156800, k = 35321811042304},
                    Board {wp = 16777216, bp = 18049583015788544, k = 18049583032565760},
                    Board {wp = 9007208465695233, bp = 549755813888, k = 549755813889},
                    Board {bp = 8856458364416,wp = 27145225982967808, k = 103080787968},
                    Board {wp = 140737555529728, bp = 45036547103522816, k = 140737488617472}]

  -- res1 <- mapM (\x -> do 
  --   a <- evaluate x Black (-mate) mate 20
  --   b <- evaluateL x Black (-mate) mate 20
  --   return (a, b, x)) $ testBoards
  -- let res = filter (\(a,b, _) -> a /= b) res1
  -- if null res then print "ok"
  -- else mapM_ (\(a, b,_) -> putStrLn $ (show a) ++ ":" ++ (show b)) res



  -- res <- evaluate initialBoard White (-mate) mate 20
  -- res1 <- evaluateL initialBoard White (-mate) mate 20
  -- putStrLn $ (show res) ++ ":" ++ (show res1)

  -- res <- evaluate Board {wp = 27021735203700736, bp = 8796663447552, k = 536870912} Black (-mate) mate 20
  -- res1 <- evaluateL Board {wp = 27021735203700736, bp = 8796663447552, k = 536870912} Black (-mate) mate 20
  -- putStrLn $ (show res) ++ ":" ++ (show res1)

  -- res <- evaluate Board {wp = 35218900255232, bp = 36028797018963968, k = 33685504} Black (-mate) mate 20
  -- res1 <- evaluateL Board {wp = 35218900255232, bp = 36028797018963968, k = 33685504} Black (-mate) mate 20
  -- putStrLn $ (show res) ++ ":" ++ (show res1)

  -- res <- evaluate Board {wp = 343597383680, bp = 9051352190156800, k = 35321811042304} Black (-mate) mate 20
  -- res1 <- evaluateL Board {wp = 343597383680, bp = 9051352190156800, k = 35321811042304} Black (-mate) mate 20
  -- putStrLn $ (show res) ++ ":" ++ (show res1)

  -- res <- evaluate Board {wp = 16777216, bp = 18049583015788544, k = 18049583032565760} Black (-mate) mate 20
  -- res1 <- evaluateL Board {wp = 16777216, bp = 18049583015788544, k = 18049583032565760} Black (-mate) mate 20
  -- putStrLn $ (show res) ++ ":" ++ (show res1)

  -- res <- evaluate Board {wp = 9007208465695233, bp = 549755813888, k = 549755813889} Black (-mate) mate 20
  -- res1 <- evaluateL Board {wp = 9007208465695233, bp = 549755813888, k = 549755813889} Black (-mate) mate 20
  -- putStrLn $ (show res) ++ ":" ++ (show res1)

  -- res <- evaluate Board {bp = 8856458364416,wp = 27145225982967808, k = 103080787968} Black (-mate) mate 20
  -- res1 <- evaluateL Board {bp = 8856458364416, wp = 27145225982967808, k = 103080787968} Black (-mate) mate 20
  -- putStrLn $ (show res) ++ ":" ++ (show res1)


  table <- allocate
  args <- getArgs
  -- progName <- getProgName
  --mapM_ putStrLn args
  -- putStrLn progName
  --let args = ["w"] -- do zakomentowania w programmie
  case (listToMaybe args) of
    Just "b" -> loopGame initialGameState table Black
    Just "w" -> loopGame initialGameState table White
    Nothing -> loopGame initialGameState table White
