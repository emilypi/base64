{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.W64.Loop
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- 'Word64'-optimized inner loops
--
module Data.ByteString.Base64.Internal.W64.Loop
( innerLoop
, innerLoopNopad
) where


import Data.Bits
import Data.ByteString.Internal
import Data.ByteString.Base64.Internal.Utils

import Foreign.Ptr
import Foreign.Storable

import GHC.Word


-- | Encoding inner loop. Packs 6 bytes from src pointer into
-- the first 6 bits of 4 'Word12''s (using the encoding table,
-- as 2 'Word12''s ), writing these to the dst pointer.
--
innerLoop
    :: Ptr Word16
    -> Ptr Word8
    -> Ptr Word64
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> IO ())
    -> IO ()
innerLoop etable !sptr !dptr !end finish = go (castPtr sptr) dptr
  where
    tailRound !src !dst
      | plusPtr src 2 >= end = finish src (castPtr dst)
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !w <- peek @Word32 (castPtr src)
#else
        !w <- byteSwap32 <$> peek @Word32 (castPtr src)
#endif
        let !a = (unsafeShiftR w 20) .&. 0xfff
            !b = (unsafeShiftR w 8) .&. 0xfff

        !x <- w32_16 <$> peekElemOff etable (fromIntegral a)
        !y <- w32_16 <$> peekElemOff etable (fromIntegral b)

        let !z = x .|. (unsafeShiftL y 16)

        poke @Word32 dst z

        finish (plusPtr src 3) (castPtr (plusPtr dst 4))
    {-# INLINE tailRound #-}

    go !src !dst
      | plusPtr src 5 >= end = tailRound (castPtr src) (castPtr dst)
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !t <- peek @Word64 src
#else
        !t <- byteSwap64 <$> peek @Word64 src
#endif
        let !a = (unsafeShiftR t 52) .&. 0xfff
            !b = (unsafeShiftR t 40) .&. 0xfff
            !c = (unsafeShiftR t 28) .&. 0xfff
            !d = (unsafeShiftR t 16) .&. 0xfff

        !w <- w64 <$> peekElemOff etable (fromIntegral a)
        !x <- w64 <$> peekElemOff etable (fromIntegral b)
        !y <- w64 <$> peekElemOff etable (fromIntegral c)
        !z <- w64 <$> peekElemOff etable (fromIntegral d)

        let !xx = w
               .|. (unsafeShiftL x 16)
               .|. (unsafeShiftL y 32)
               .|. (unsafeShiftL z 48)

        poke dst (fromIntegral xx)

        go (plusPtr src 6) (plusPtr dst 8)
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
    tailRound !src !dst !n
      | plusPtr src 2 >= end = finish src (castPtr dst) n
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !w <- peek @Word32 (castPtr src)
#else
        !w <- byteSwap32 <$> peek @Word32 (castPtr src)
#endif
        let !a = (unsafeShiftR w 20) .&. 0xfff
            !b = (unsafeShiftR w 8) .&. 0xfff

        !x <- w32_16 <$> peekElemOff etable (fromIntegral a)
        !y <- w32_16 <$> peekElemOff etable (fromIntegral b)

        let !z = x .|. (unsafeShiftL y 16)

        poke @Word32 dst z

        finish (plusPtr src 3) (castPtr (plusPtr dst 4)) (n + 4)

    go !src !dst !n
      | plusPtr src 5 >= end = tailRound (castPtr src) (castPtr dst) n
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !t <- peek @Word64 src
#else
        !t <- byteSwap64 <$> peek @Word64 src
#endif
        let !a = (unsafeShiftR t 52) .&. 0xfff
            !b = (unsafeShiftR t 40) .&. 0xfff
            !c = (unsafeShiftR t 28) .&. 0xfff
            !d = (unsafeShiftR t 16) .&. 0xfff

        w <- peekElemOff etable (fromIntegral a)
        x <- peekElemOff etable (fromIntegral b)
        y <- peekElemOff etable (fromIntegral c)
        z <- peekElemOff etable (fromIntegral d)


        poke dst w
        poke (plusPtr dst 2) x
        poke (plusPtr dst 4) y
        poke (plusPtr dst 6) z

        go (plusPtr src 6) (plusPtr dst 8) (n + 8)
{-# INLINE innerLoopNopad #-}
