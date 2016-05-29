module Masks where

import           Data.Bits
import qualified Data.Vector.Unboxed as V
import           Data.Word
import Data.List


invalid :: Word64
invalid = 64 :: Word64

mapBoard :: Int -> Word64 -- maps board
mapBoard i = V.fromList [invalid, field 29, invalid, field 30, invalid, field 31, invalid, field 32,
             field 25, invalid, field 26, invalid, field 27, invalid, field 28, invalid,
             invalid, field 21, invalid, field 22, invalid, field 23, invalid, field 24,
             field 17, invalid, field 18, invalid, field 19, invalid, field 20, invalid,
             invalid, field 13, invalid, field 14, invalid, field 15, invalid, field 16,
             field 9, invalid, field 10, invalid, field 11, invalid, field 12, invalid,
             invalid, field 5, invalid, field 6, invalid, field 7, invalid, field 8,
             field 1, invalid, field 2, invalid, field 3, invalid, field 4, invalid] V.! i


kBoard :: Int -> Int -- board mapping
kBoard i = V.fromList [30,33,33,33,33,34,34,33,34,36,36,35,36,38,38,34,34,38,38,36,35,36,36,34,33,34,34,33,33,33,33,30] V.! (i - 1)


bitsOnTheBoard :: Word64
bitsOnTheBoard = mergeBoardFields [1..32]


mergeBoardFields :: [Int] -> Word64
mergeBoardFields = foldl' (.|.) 0 . getBoardFields


getBoardFields :: [Int] -> [Word64]
getBoardFields = map field


getIndex :: Word64 -> Word64
getIndex x
  | x == 0 = 0
  | otherwise = fromIntegral $ 63 - countLeadingZeros x

s :: Word64 -> Word64 -- get powers of two
s i = if i < 64 then unsafeShiftL 1 (fromIntegral i) else 0


field :: Int -> Word64
field 0 = 0
field 1 = s 0
field 2 = s 8
field 3 = s 16
field 4 = s 24

field 5 = s 9
field 6 = s 17
field 7 = s 25
field 8 = s 33

field 9 = s 10
field 10 = s 18
field 11 = s 26
field 12 = s 34

field 13 = s 19
field 14 = s 27
field 15 = s 35
field 16 = s 43

field 17 = s 20
field 18 = s 28
field 19 = s 36
field 20 = s 44

field 21 = s 29
field 22 = s 37
field 23 = s 45
field 24 = s 53

field 25 = s 30
field 26 = s 38
field 27 = s 46
field 28 = s 54

field 29 = s 39
field 30 = s 47
field 31 = s 55
field 32 = s 63

field _ = invalid


rfield :: Word64 -> Int
rfield 0 = 0
rfield 1 = 1
rfield 256 = 2
rfield 65536 = 3
rfield 16777216 = 4

rfield 512 = 5
rfield 131072 = 6
rfield 33554432 = 7
rfield 8589934592 = 8

rfield 1024 = 9
rfield 262144 = 10
rfield 67108864 = 11
rfield 17179869184 = 12

rfield 524288 = 13
rfield 134217728 = 14
rfield 34359738368 = 15
rfield 8796093022208 = 16

rfield 1048576 = 17
rfield 268435456 = 18
rfield 68719476736 = 19
rfield 17592186044416 = 20

rfield 536870912 = 21
rfield 137438953472 = 22
rfield 35184372088832 = 23
rfield 9007199254740992 = 24

rfield 1073741824 = 25
rfield 274877906944 = 26
rfield 70368744177664 = 27
rfield 18014398509481984 = 28

rfield 549755813888 = 29
rfield 140737488355328 = 30
rfield 36028797018963968 = 31
rfield 9223372036854775808 = 32

rfield _ = 0
