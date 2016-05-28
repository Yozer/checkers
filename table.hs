module Table where
import Board
import qualified Data.Vector.Mutable as VM
import qualified            Data.Vector as V

data TTFlag = Exact | LowerBound | UpperBound deriving (Show, Eq)

data TTEntry = TTEntry {
  value :: {-# UNPACK #-} !Int,
  bestMove :: {-# UNPACK #-} !MoveHolder,
  flag :: {-# UNPACK #-} !TTFlag,
  depth  :: {-# UNPACK #-} !Int
} | TTNone deriving (Show, Eq) 

size :: Int
size = 700000

type TTable = V.Vector TTEntry

allocate :: IO TTable
allocate = do
  vector <- VM.replicate size TTNone
  V.unsafeFreeze vector


clear :: TTable -> IO TTable
clear v = do
  vector <- V.unsafeThaw v
  VM.clear vector
  VM.set vector TTNone
  V.unsafeFreeze vector 

readTT :: Int -> TTable -> TTEntry
readTT hash v = v V.! hash


writeTT :: Int -> TTEntry -> TTable -> IO TTable
writeTT hash entry v = do
  vector <- V.unsafeThaw v
  VM.write vector hash entry
  V.unsafeFreeze vector 

