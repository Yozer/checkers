module Ai where

import           Board
import           Control.Parallel.Strategies
import           Eval
import           Moves


-- class Node a where
--   children :: a -> [a]
--   evaluate :: a -> Float

-- data NodeTree a = Branch [NodeTree a] | Leaf a


-- depthLimit :: (Node a) => Int -> a -> NodeTree a
-- depthLimit 0 n        = Leaf n
-- depthLimit c n
--   | null (children n) = Leaf n
--   | otherwise         = Branch $ map (depthLimit (c-1)) (children n)


-- mmSearch :: (Node a) => Float -> Float -> NodeTree a -> Float
-- mmSearch a b (Leaf n)        = evaluate n
-- mmSearch a b (Branch (e:es)) = prune a b (mmSearch (-b) (-a) e) es
--   where prune a b e es
--           | b < e     = -e
--           | null es   = -max a e
--           | otherwise = mmSearch (max a e) b (Branch es)


-- alphabeta :: Int -> AlphaInfo -> AlphaResult
-- alphabeta c (AlphaInfo board player) = maximum $ map (\x -> (negate $ mmSearch (-1/0.0) (1/0.0) (depthLimit c (AlphaInfo (doMove board player x) (getNextPlayer player))), x)) actions
--   where
--      actions = getActions board player

-- data AlphaInfo = AlphaInfo Board Player
-- instance Node AlphaInfo where
--   evaluate (AlphaInfo board player) = E.evaluate board player
--   children (AlphaInfo board player) = map (\x -> AlphaInfo (doMove board player x) (getNextPlayer player)) actions
--     where
--      actions = getActions board player





type AlphaResult = (Float, MoveHolder)

maxDeep :: Int
maxDeep = 15

alphaBeta :: Board -> Player -> AlphaResult
alphaBeta board player = minimum $  parMap rpar (\x -> (alphaBeta' (doMove board player x) (getNextPlayer player) maxDeep (-maxEval) maxEval, x)) actions
  where
    actions = getActions board player


alphaBeta' :: Board -> Player -> Int -> Float -> Float -> Float
alphaBeta' board player depth alpha beta
  | depth == 0 || isGameEnded board = evaluate board player
  | null actions = if player == White then -maxEval else maxEval
  | otherwise = makeMoves actions board depth player (-maxEval) alpha beta
  where
    actions = getActions board player


makeMoves :: [MoveHolder] -> Board -> Int -> Player -> Float -> Float -> Float -> Float
makeMoves (move:restMoves) board depth player best alpha beta
  | null restMoves || pruning = best'
  | otherwise = makeMoves restMoves board depth player best' alpha' beta
  where
    nextBoard = doMove board player move
    value = negate $ alphaBeta' nextBoard (getNextPlayer player) (depth-1) (-beta) (-alpha)
    best' = if value > best then value else best
    alpha' = if value > alpha then value else alpha
    pruning = alpha' >= beta

makeMoves [] _ _ _ best _ _= best

getNextPlayer :: Player -> Player
getNextPlayer x = if x == White then Black else White
