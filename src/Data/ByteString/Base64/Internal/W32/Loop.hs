{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.W32.Loop
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- 'Word32'-optimized inner loop
--
module Data.ByteString.Base64.Internal.W32.Loop
( innerLoop
, innerLoopNopad
) where


import Data.Bits
import Data.ByteString.Internal
import Data.ByteString.Base64.Internal.Utils

import Foreign.Ptr
import Foreign.Storable

import GHC.Word


-- | Encoding inner loop. Packs 3 bytes from src pointer into
-- the first 6 bytes of 4 'Word8''s (using the encoding table,
-- as 2 'Word12''s ), writing these to the dst pointer.
--
innerLoop
    :: Ptr Word16
    -> Ptr Word8
    -> Ptr Word32
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> IO ())
    -> IO ()
innerLoop !etable !sptr !dptr !end finish = go (castPtr sptr) dptr
  where
    go !src !dst
      | plusPtr src 2 >= end = finish (castPtr src) (castPtr dst)
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !w <- peek @Word32 src
#else
        !w <- byteSwap32 <$> peek @Word32 src
#endif
        let !a = (unsafeShiftR w 20) .&. 0xfff
            !b = (unsafeShiftR w 8) .&. 0xfff

        !x <- w32_16 <$> peekElemOff etable (fromIntegral a)
        !y <- w32_16 <$> peekElemOff etable (fromIntegral b)

        let !z = x .|. (unsafeShiftL y 16)
        poke dst (fromIntegral z)

        go (plusPtr src 3) (plusPtr dst 4)
{-# INLINE innerLoop #-}

-- | Unpadded encoding loop, finalized as a bytestring using the
-- resultant length count.
--
innerLoopNopad
    :: Ptr Word16
    -> Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> Int -> IO ByteString)
    -> IO ByteString
innerLoopNopad !etable !sptr !dptr !end finish = go (castPtr sptr) dptr 0
  where
    go !src !dst !n
      | plusPtr src 2 >= end = finish (castPtr src) (castPtr dst) n
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        w <- peek @Word32 src
#else
        w <- byteSwap32 <$> peek @Word32 src
#endif
        let !a = (unsafeShiftR w 20) .&. 0xfff
            !b = (unsafeShiftR w 8) .&. 0xfff

        !x <- peekElemOff etable (fromIntegral a)
        !y <- peekElemOff etable (fromIntegral b)

        poke dst x
        poke (plusPtr dst 2) y

        go (plusPtr src 3) (plusPtr dst 4) (n + 4)
{-# INLINE innerLoopNopad #-}
