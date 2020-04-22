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
    -> (Ptr Word8 -> Ptr Word8 -> Int -> IO ByteString)
    -> Int
    -> IO ByteString
innerLoop !etable !sptr !dptr !end finish !nn = go sptr dptr nn
  where
    go !src !dst !n
      | plusPtr src 7 >= end =
        W32.innerLoop etable (castPtr src) (castPtr dst) (castPtr end) finish n
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

        go (plusPtr src 6) (plusPtr dst 8) (n + 8)
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
    -> (Ptr Word8 -> Ptr Word8 -> Int -> IO (Either Text ByteString))
        -- ^ dst foreign ptr (for consing bs)
    -> Int
    -> IO (Either Text ByteString)
decodeLoop = W16.decodeLoop
--   where
--     err p = return . Left . T.pack
--       $ "invalid character at offset: "
--       ++ show (p `minusPtr` sptr)

--     padErr p =  return . Left . T.pack
--       $ "invalid padding at offset: "
--       ++ show (p `minusPtr` sptr)

--     go !dst !src !n
--       | plusPtr src 8 >= end =
--         W32.decodeLoop dtable (castPtr src) dst (castPtr end) dfp n
--       | otherwise = do
-- #ifdef WORDS_BIGENDIAN
--         !tt <- peek @Word64 src
-- #else
--         !tt <- byteSwap64 <$> peek @Word64 src
-- #endif
--         let !s = fromIntegral ((unsafeShiftR tt 56) .&. 0xff)
--             !t = fromIntegral ((unsafeShiftR tt 48) .&. 0xff)
--             !u = fromIntegral ((unsafeShiftR tt 40) .&. 0xff)
--             !v = fromIntegral ((unsafeShiftR tt 32) .&. 0xff)
--             !w = fromIntegral ((unsafeShiftR tt 24) .&. 0xff)
--             !x = fromIntegral ((unsafeShiftR tt 16) .&. 0xff)
--             !y = fromIntegral ((unsafeShiftR tt 8) .&. 0xff)
--             !z = fromIntegral (tt .&. 0xff)

--         !a <- w64 <$> peekByteOff dtable s
--         !b <- w64 <$> peekByteOff dtable t
--         !c <- w64 <$> peekByteOff dtable u
--         !d <- w64 <$> peekByteOff dtable v
--         !e <- w64 <$> peekByteOff dtable w
--         !f <- w64 <$> peekByteOff dtable x
--         !g <- w64 <$> peekByteOff dtable y
--         !h <- w64 <$> peekByteOff dtable z

--         if
--           | a == 0x63 -> padErr src
--           | b == 0x63 -> padErr (plusPtr src 1)
--           | c == 0x63 -> padErr (plusPtr src 2)
--           | d == 0x63 -> padErr (plusPtr src 3)
--           | e == 0x63 -> padErr (plusPtr src 4)
--           | f == 0x63 -> padErr (plusPtr src 5)
--           | g == 0x63 -> padErr (plusPtr src 6)
--           | h == 0x63 -> padErr (plusPtr src 7)
--           | a == 0xff -> err src
--           | b == 0xff -> err (plusPtr src 1)
--           | c == 0xff -> err (plusPtr src 2)
--           | d == 0xff -> err (plusPtr src 3)
--           | e == 0xff -> err (plusPtr src 4)
--           | f == 0xff -> err (plusPtr src 5)
--           | g == 0xff -> err (plusPtr src 6)
--           | h == 0xff -> err (plusPtr src 7)
--           | otherwise -> do

--             let !xx = (unsafeShiftL a 18)
--                   .|. (unsafeShiftL b 12)
--                   .|. (unsafeShiftL c 6)
--                   .|. d

--                 !yy = (unsafeShiftL e 18)
--                   .|. (unsafeShiftL f 12)
--                   .|. (unsafeShiftL g 6)
--                   .|. h

--             poke @Word8 dst (fromIntegral (unsafeShiftR xx 16))
--             poke @Word8 (plusPtr dst 1) (fromIntegral (unsafeShiftR xx 8))
--             poke @Word8 (plusPtr dst 2) (fromIntegral xx)
--             poke @Word8 (plusPtr dst 3) (fromIntegral (unsafeShiftR yy 16))
--             poke @Word8 (plusPtr dst 4) (fromIntegral (unsafeShiftR yy 8))
--             poke @Word8 (plusPtr dst 5) (fromIntegral yy)

--             go (plusPtr dst 6) (plusPtr src 8) (n + 6)
{-# INLINE decodeLoop #-}

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
