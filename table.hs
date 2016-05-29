module Table where
import Board
import qualified Data.Vector.Mutable as VM
import qualified            Data.Vector as V
import Data.Word
import Control.Monad.ST
import Data.IORef
import Control.Concurrent.MVar

data TTFlag = Exact | LowerBound | UpperBound deriving (Show, Eq)

data TTEntry = TTEntry {
  tValue :: {-# UNPACK #-} !Int,
  tFlag :: {-# UNPACK #-} !TTFlag,
  tDepth  :: {-# UNPACK #-} !Int,
  tHash :: {-# UNPACK #-} !Word64
} | TTNone deriving (Show, Eq) 

size :: Int
size = 3000000

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

readTT :: Word64 -> Int -> TTableRef -> IO TTEntry
readTT hash depth ref = do
  v <- readIORef ref
  value <- VM.read v (indexFromHash hash)
  if value == TTNone || tHash value /= hash || tDepth value < depth then return TTNone else return value

writeTT :: Word64 -> TTEntry -> TTableRef -> IO Bool
writeTT hash entry ref = do
  v <- readIORef ref
  VM.write v (indexFromHash hash) entry
  return True

indexFromHash :: Word64 -> Int
indexFromHash h = (fromIntegral $ h `mod` (fromIntegral size))
