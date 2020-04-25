{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
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
, decodeLoop
, lenientLoop
) where


import Data.Bits
import Data.ByteString.Internal
import Data.ByteString.Base64.Internal.Utils
import qualified Data.ByteString.Base64.Internal.W16.Loop as W16
import qualified Data.ByteString.Base64.Internal.W32.Loop as W32
import Data.Text (Text)

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Word


-- | Encoding inner loop. Packs 6 bytes from src pointer into
-- the first 6 bits of 4 'Word12''s (using the encoding table,
-- as 2 'Word12''s ), writing these to the dst pointer.
--
innerLoop
    :: Ptr Word16
    -> Ptr Word64
    -> Ptr Word64
    -> Ptr Word64
    -> (Ptr Word8 -> Ptr Word8 -> IO ByteString)
    -> IO ByteString
innerLoop !etable !sptr !dptr !end finish = go sptr dptr
  where
    go !src !dst
      | plusPtr src 7 >= end =
        W32.innerLoop etable (castPtr src) (castPtr dst) (castPtr end) finish
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

        !w <- w64_16 <$> peekElemOff etable (fromIntegral a)
        !x <- w64_16 <$> peekElemOff etable (fromIntegral b)
        !y <- w64_16 <$> peekElemOff etable (fromIntegral c)
        !z <- w64_16 <$> peekElemOff etable (fromIntegral d)

        let !xx = w
               .|. (unsafeShiftL x 16)
               .|. (unsafeShiftL y 32)
               .|. (unsafeShiftL z 48)

        poke dst (fromIntegral xx)

        go (plusPtr src 6) (plusPtr dst 8)
    {-# INLINE go #-}
{-# INLINE innerLoop #-}

decodeLoop
    :: Ptr Word8
        -- ^ decode lookup table
    -> Ptr Word8
        -- ^ src pointer
    -> Ptr Word8
        -- ^ dst pointer
    -> Ptr Word8
        -- ^ end of src ptr
    -> (Ptr Word8 -> Ptr Word8 -> IO (Either Text ByteString))
        -- ^ dst foreign ptr (for consing bs)
    -> IO (Either Text ByteString)
decodeLoop = W16.decodeLoop

lenientLoop
    :: Ptr Word8
        -- ^ decode lookup table
    -> Ptr Word8
        -- ^ src pointer
    -> Ptr Word8
        -- ^ dst pointer
    -> Ptr Word8
        -- ^ end of src ptr
    -> ForeignPtr Word8
        -- ^ dst foreign ptr (for consing bs)
    -> IO ByteString
lenientLoop = W16.lenientLoop
