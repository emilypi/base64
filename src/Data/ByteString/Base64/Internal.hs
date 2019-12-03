{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
module Data.ByteString.Base64.Internal
( base64
, base64'
) where


import Control.Monad (when)

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Internal

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Exts
import GHC.Storable
import GHC.Word




base64 :: Addr# -> Bool -> ByteString -> ByteString
base64 alpha padding (PS fp _ l) =
    unsafeCreate dlen $ \dst ->
      withForeignPtr fp $ \src ->
        base64Internal alpha src dst l padding
  where
    dlen =
      let (q,r) = l `divMod` 3
      in 4 * (if r == 0 then q else q+1)

base64' :: Addr# -> Bool -> ByteString -> ByteString
base64' alpha padding (PS fp _ l) =
    unsafeCreate dlen $ \dst ->
      withForeignPtr fp $ \src ->
        base64Internal' alpha src dst l padding
  where
    dlen =
      let (q,r) = l `divMod` 3
      in 4 * (if r == 0 then q else q+1)


base64Internal :: Addr# -> Ptr Word8 -> Ptr Word8 -> Int -> Bool -> IO ()
base64Internal alpha src dst slen padded = go 0 0
  where
    _eq = 0x3d :: Word8

    go !i !j
      | i >= slen = return ()
      | otherwise = do
        a <- peekByteOff src i
        b <- if i + 1 >= slen then return 0 else peekByteOff src (i + 1)
        c <- if i + 2 >= slen then return 0 else peekByteOff src (i + 2)

        let (!w, !x, !y, !z) = encodeTriplet alpha a b c

        pokeByteOff dst j w
        pokeByteOff dst (j + 1) x

        if i + 1 < slen
        then pokeByteOff dst (j + 2) y
        else when padded (pokeByteOff dst (j + 2) _eq)

        if i + 2 < slen
        then pokeByteOff dst (j + 3) z
        else when padded (pokeByteOff dst (j + 3) _eq)

        go (i + 3) (j + 4)

base64Internal' :: Addr# -> Ptr Word8 -> Ptr Word8 -> Int -> Bool -> IO ()
base64Internal' alpha src dst slen padded = go 0 0
  where
    _eq = 0x3d :: Word8

    src32 = castPtr src

    go :: Int -> Int -> IO ()
    go i j
      | i >= slen = return ()
      | otherwise = do
        x <- readWord32OffPtr src32 i

        let (!a, !b, !c, !d) = encodeTriplet' alpha x

        pokeByteOff dst j a
        pokeByteOff dst j b

        if i + 1 < slen
        then pokeByteOff dst (j + 2) c
        else when padded (pokeByteOff dst (j + 2) _eq)

        if i + 2 < slen
        then pokeByteOff dst (j + 3) d
        else when padded (pokeByteOff dst (j + 3) _eq)

        go (i + 3) (j + 4)



-- | Naive implemementation. We can do better by copying directly to a 'Word32'
--
-- See: http://0x80.pl/notesen/2015-12-27-base64-encoding.html
--
encodeTriplet :: Addr# -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8)
encodeTriplet alpha a b c =
    let
      !w = shiftR a 2
      !x = (shiftL a 4 .&. 0x30) .|. (shiftR b 4)
      !y = (shiftL b 2 .&. 0x3c) .|. (shiftR c 6)
      !z = c .&. 0x3f
    in (ix w, ix x, ix y, ix z)
  where
    ix (W8# i) = W8# (indexWord8OffAddr# alpha (word2Int# i))
{-# INLINE encodeTriplet #-}

encodeTriplet' :: Addr# -> Word32 -> (Word8, Word8, Word8, Word8)
encodeTriplet' alpha x =
    let
      !a = fromIntegral (x .&. 0x3f)
      !b = fromIntegral ((shiftR x 8) .&. 0x3f)
      !c = fromIntegral ((shiftR x 16) .&. 0x3f)
      !d = fromIntegral ((shiftR x 24) .&. 0x3f)
    in (ix a, ix b, ix c, ix d)
  where
    ix (W8# i) = W8# (indexWord8OffAddr# alpha (word2Int# i))
{-# INLINE encodeTriplet' #-}
