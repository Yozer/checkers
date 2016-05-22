module Tests where

import           Test.HUnit
import qualified TestMoves  as TM
import qualified TestMovesListWhite as TMLW
import qualified TestMovesListBlack as TMLB
import Eval
import Ai

main :: IO Counts
main = runTestTT $ TestList (TM.getTestList ++ TMLW.getTestList ++ TMLB.getTestList)
