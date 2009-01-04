{-

Module : System.ByteOrder
Copyright : (c) Antoine Latter 2009
License : BSD3

Maintainer : Antoine Latter <aslatter@gmail.com>
Stability: unstable
Portability: requires FFI

-}
module System.ByteOrder(byteOrder, ByteOrder(..)) where

import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Foreign

import Data.Word

-- |Indicates the byte-ordering for a 4-byte value, where '1'
-- indicates the most-significant byte and '4' indicates the
-- least significant byte.
--
-- In this format, big endian byte order would be represented as:
-- (1,2,3,4).
--
-- For convinience, the most common cases (BigEndian and LittleEndian)
-- are provided their own constructors.
data ByteOrder
    = BigEndian
    | LittleEndian
    | Mixed (Word8, Word8, Word8, Word8)
 deriving (Eq, Show, Read, Ord)

input :: Word32
input = 0x01020304

-- |Returns the native byte ordering of the system.
byteOrder :: ByteOrder
byteOrder = unsafePerformIO $ byteOrderIO

byteOrderIO :: IO ByteOrder
byteOrderIO = byteListToByteOrder `fmap` wordToByteList input

wordToByteList :: Word32 -> IO [Word8]
wordToByteList word = alloca $ \wordPtr -> do
         poke wordPtr word
         peekArray 4 (castPtr wordPtr)

byteListToByteOrder :: [Word8] -> ByteOrder
byteListToByteOrder [1, 2, 3, 4] = BigEndian
byteListToByteOrder [4, 3, 2, 1] = LittleEndian
byteListToByteOrder xs@[x1, x2, x3, x4] = Mixed (x1,x2,x3,x4)

