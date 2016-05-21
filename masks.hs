module Masks where

import           Data.Bits
import qualified Data.Vector.Unboxed as V
import           Data.Word




invalid :: Word64
invalid = 64 :: Word64

mapBoard :: Int -> Word64 -- maps board
mapBoard i = V.fromList [invalid, field_29, invalid, field_30, invalid, field_31, invalid, field_32,
             field_25, invalid, field_26, invalid, field_27, invalid, field_28, invalid,
             invalid, field_21, invalid, field_22, invalid, field_23, invalid, field_24,
             field_17, invalid, field_18, invalid, field_19, invalid, field_20, invalid,
             invalid, field_13, invalid, field_14, invalid, field_15, invalid, field_16,
             field_9, invalid, field_10, invalid, field_11, invalid, field_12, invalid,
             invalid, field_5, invalid, field_6, invalid, field_7, invalid, field_8,
             field_1, invalid, field_2, invalid, field_3, invalid, field_4, invalid] V.! i

bitsOnTheBoard :: Word64
bitsOnTheBoard = field_1 .|. field_2 .|. field_3 .|. field_4 .|. field_5 .|. field_6 .|. field_7 .|. field_8 .|. field_9 .|. field_10 .|. field_11 .|. field_12 .|.
                 field_13 .|. field_14 .|. field_15 .|. field_16 .|. field_17 .|. field_18 .|. field_19 .|. field_20 .|.
                 field_32 .|. field_31 .|. field_30 .|. field_29 .|. field_28 .|. field_27 .|. field_26 .|. field_25 .|. field_24 .|. field_23 .|. field_22 .|. field_21

getIndex :: Word64 -> Word64
getIndex x
  | x == 0 = 0
  | otherwise = fromIntegral $ 63 - countLeadingZeros x


s :: Word64 -> Word64 -- get powers of two
s i = if i < 64 then unsafeShiftL 1 (fromIntegral i) else 0

field_1 = s 0
field_2 = s 8
field_3 = s 16
field_4 = s 24

field_5 = s 9
field_6 = s 17
field_7 = s 25
field_8 = s 33

field_9 = s 10
field_10 = s 18
field_11 = s 26
field_12 = s 34

field_13 = s 19
field_14 = s 27
field_15 = s 35
field_16 = s 43

field_17 = s 20
field_18 = s 28
field_19 = s 36
field_20 = s 44

field_21 = s 29
field_22 = s 37
field_23 = s 45
field_24 = s 53

field_25 = s 30
field_26 = s 38
field_27 = s 46
field_28 = s 54

field_29 = s 39
field_30 = s 47
field_31 = s 55
field_32 = s 63

rfield 1 = 1
rfield 256 = 2
rfield 65536 = 3
rfield 16777216 = 4

rfield 512= 5
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

rfield _ = -1