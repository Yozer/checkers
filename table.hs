module Table where
import           Board
import           Control.Concurrent.MVar
import           Control.Monad.ST
import qualified Data.Vector.Mutable     as VM
import           Data.Word

data TTFlag = Exact | LowerBound | UpperBound deriving (Show, Eq)

data TTEntry = TTEntry {
  tValue :: Int,
  tFlag  :: TTFlag,
  tDepth :: Int,
  tHash  :: Word64,
  tMove  :: MoveHolder
} | TTNone deriving (Show, Eq)

size :: Int
size = 7000000

type TTable = VM.MVector RealWorld TTEntry
type TTableRef = MVar TTable

allocate :: IO TTableRef
allocate = do
  v <- VM.replicate size TTNone
  newMVar  v

readTT :: Word64 -> TTableRef -> IO TTEntry
readTT hash ref = do
  v <- takeMVar ref
  value <- VM.read v (indexFromHash hash)
  putMVar ref v
  if value == TTNone || tHash value /= hash then return TTNone
  else return value


writeTT :: Word64 -> TTEntry -> TTableRef -> IO ()
writeTT hash entry ref = do
  v <- takeMVar ref
  VM.write v (indexFromHash hash) entry
  putMVar ref v

indexFromHash :: Word64 -> Int
indexFromHash h = (fromIntegral $ h `mod` (fromIntegral size))
