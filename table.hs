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
size = 2000000

type TTable = VM.MVector RealWorld TTEntry
type TTableRef = IORef TTable

allocate :: IO TTableRef
allocate = do
  v <- VM.replicate size TTNone
  newIORef v


-- clear :: TTableRef -> IO TTableRef
-- clear ref = runST $ do
--   v <- readIORef ref
--   vector <- V.unsafeThaw v
--   VM.clear vector
--   VM.set vector TTNone
--   newV <- V.unsafeFreeze vector

readTT :: Word64 -> TTableRef -> IO TTEntry
readTT hash ref = do
  v <- readIORef ref
  value <- VM.read v (indexFromHash hash)
  if value == TTNone || tHash value /= hash then return TTNone
  else return value


writeTT :: Word64 -> TTEntry -> TTableRef -> IO Bool
writeTT hash entry ref = do
  let newDepth = tDepth entry

  v <- readIORef ref
  --value <- VM.read v (indexFromHash hash)
  VM.write v (indexFromHash hash) entry
  return True

indexFromHash :: Word64 -> Int
indexFromHash h = (fromIntegral $ h `mod` (fromIntegral size))
