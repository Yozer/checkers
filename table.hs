module Table where
import           Board
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.ST
import           Data.IORef
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as VM
import           Data.Word

data TTFlag = Exact | LowerBound | UpperBound deriving (Show, Eq)

data TTEntry = TTEntry {
  tValue :: {-# UNPACK #-} !Int,
  tFlag  :: {-# UNPACK #-} !TTFlag,
  tDepth :: {-# UNPACK #-} !Int,
  tHash  :: {-# UNPACK #-} !Word64,
  tMove  :: {-# UNPACK #-} !MoveHolder
} | TTNone deriving (Show, Eq)

size :: Int
size = 10000000

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


writeTT :: Word64 -> TTEntry -> TTableRef -> IO Bool
writeTT hash entry ref = do
  --let newDepth = tDepth entry

  v <- takeMVar ref
  --value <- VM.read v (indexFromHash hash)
  VM.write v (indexFromHash hash) entry
  putMVar ref v
  return True

indexFromHash :: Word64 -> Int
indexFromHash h = (fromIntegral $ h `mod` (fromIntegral size))
