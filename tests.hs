module Tests where


import           Test.HUnit
import qualified TestBoard          as TB
import qualified TestMoves          as TM
import qualified TestMovesListBlack as TMLB
import qualified TestMovesListWhite as TMLW

main :: IO Counts
main = runTestTT $ TestList (TM.getTestList ++ TMLW.getTestList ++ TMLB.getTestList ++ TB.getTestList)
