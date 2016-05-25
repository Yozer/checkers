module Ai where

import           Board
import           Control.Parallel.Strategies
import qualified           Eval as E
import           Moves


class Node a where
  children :: a -> [a]
  evaluate :: a -> Float

data NodeTree a = Branch [NodeTree a] | Leaf a


depthLimit :: (Node a) => Int -> a -> NodeTree a
depthLimit 0 n        = Leaf n
depthLimit c n
  | null (children n) = Leaf n
  | otherwise         = Branch $ map (depthLimit (c-1)) (children n)


mmSearch :: (Node a) => Float -> Float -> NodeTree a -> Float
mmSearch a b (Leaf n)        = evaluate n
mmSearch a b (Branch (e:es)) = prune a b (mmSearch (-b) (-a) e) es
  where prune a b e es
          | b < e     = -e
          | null es   = -max a e
          | otherwise = mmSearch (max a e) b (Branch es)


alphabeta :: Int -> AlphaInfo -> AlphaResult
alphabeta c (AlphaInfo board player) = maximum $ map (\x -> (negate $ mmSearch (-1/0.0) (1/0.0) (depthLimit c (AlphaInfo (doMove board player x) (getNextPlayer player))), x)) actions
  where
     actions = getActions board player

data AlphaInfo = AlphaInfo Board Player
instance Node AlphaInfo where
  evaluate (AlphaInfo board player) = E.evaluate board player
  children (AlphaInfo board player) = map (\x -> AlphaInfo (doMove board player x) (getNextPlayer player)) actions
    where
     actions = getActions board player





type AlphaResult = (Float, MoveHolder)

-- maxDeep :: Int
-- maxDeep = 10

-- alphaBeta :: Board -> AlphaResult
-- alphaBeta board = maximum $  parMap rpar (\x -> (negate $ alphaBeta' (doMove board White x) Black maxDeep, x)) actions
--   where
--     actions = getActions board White


-- alphaBeta' :: Board -> Player -> Int -> Float
-- alphaBeta' board player depth
--   | isGameEnded board || null actions = -maxEval - 1
--   | depth == 0  = evaluate board player
--   | otherwise = makeMoves actions board depth player (-maxEval - 1)
--   where
--     actions = getActions board player


-- makeMoves :: [MoveHolder] -> Board -> Int -> Player -> Float -> Float
-- makeMoves (move:restMoves) board depth player best
--   | null restMoves = best'
--   | otherwise = makeMoves restMoves board depth player best'
--   where
--     nextBoard = doMove board player move
--     value = negate $ alphaBeta' nextBoard (getNextPlayer player) (depth-1)
--     best' = if value > best then value else best
--     --alpha' = if best' > alpha then best' else alpha
--     --pruning = best' >= beta

-- makeMoves [] _ _ _ best = best

getNextPlayer :: Player -> Player
getNextPlayer x = if x == White then Black else White
