{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
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
, decodeLoop
, lenientLoop
) where


import Data.Bits
import Data.ByteString.Internal
import Data.ByteString.Base64.Internal.Utils
import qualified Data.ByteString.Base64.Internal.W16.Loop as W16
import Data.Text (Text)
import qualified Data.Text as T

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Exts
import GHC.Word


-- | Encoding inner loop. Packs 3 bytes from src pointer into
-- the first 6 bytes of 4 'Word8''s (using the encoding table,
-- as 2 'Word12''s ), writing these to the dst pointer.
--
innerLoop
    :: Ptr Word16
    -> Ptr Word32
    -> Ptr Word32
    -> Ptr Word32
    -> (Ptr Word8 -> Ptr Word8 -> Int -> IO ByteString)
    -> Int
    -> IO ByteString
innerLoop !etable !sptr !dptr !end finish !nn = go sptr dptr nn
  where
    go !src !dst !n
      | plusPtr src 3 >= end =
        W16.innerLoop etable (castPtr src) (castPtr dst) (castPtr end) finish n
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

        go (plusPtr src 3) (plusPtr dst 4) (n + 4)
    {-# INLINE go #-}
{-# INLINE innerLoop #-}

decodeLoop
    :: Addr#
    -> Ptr Word32
        -- ^ src pointer
    -> Ptr Word8
        -- ^ dst pointer
    -> Ptr Word32
        -- ^ end of src ptr
    -> ForeignPtr Word8
        -- ^ dst foreign ptr (for consing bs)
    -> Int
    -> IO (Either Text ByteString)
decodeLoop !dtable !sptr !dptr !end !dfp !nn = go dptr sptr nn
  where
    err p = return . Left . T.pack
      $ "invalid character at offset: "
      ++ show (p `minusPtr` sptr)

    padErr p =  return . Left . T.pack
      $ "invalid padding at offset: "
      ++ show (p `minusPtr` sptr)

    lix !i = w32 (aix (fromIntegral i) dtable)

    go !dst !src !n
      | plusPtr src 4 >= end =
        W16.decodeLoop dtable (castPtr src) (castPtr dst) (castPtr end) dfp n
      | otherwise = do
#ifdef WORDS_BIGENDIAN
        !t <- peek @Word32 src
#else
        !t <- byteSwap32 <$> peek @Word32 src
#endif
        let !a = lix ((unsafeShiftR t 24) .&. 0xff)
            !b = lix ((unsafeShiftR t 16) .&. 0xff)
            !c = lix ((unsafeShiftR t 8) .&. 0xff)
            !d = lix (t .&. 0xff)

        if
          | a == 0x63 -> padErr src
          | b == 0x63 -> padErr (plusPtr src 1)
          | c == 0x63 -> padErr (plusPtr src 2)
          | d == 0x63 -> padErr (plusPtr src 3)
          | a == 0xff -> err src
          | b == 0xff -> err (plusPtr src 1)
          | c == 0xff -> err (plusPtr src 2)
          | d == 0xff -> err (plusPtr src 3)
          | otherwise -> do

            let !xx = (unsafeShiftL a 18)
                  .|. (unsafeShiftL b 12)
                  .|. (unsafeShiftL c 6)
                  .|. d

            poke @Word8 dst (fromIntegral (unsafeShiftR xx 16))
            poke @Word8 (plusPtr dst 1) (fromIntegral (unsafeShiftR xx 8))
            poke @Word8 (plusPtr dst 2) (fromIntegral xx)
            go (plusPtr dst 3) (plusPtr src 4) (n + 3)
    {-# INLINE go #-}
{-# INLINE decodeLoop #-}

lenientLoop
    :: Addr#
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
