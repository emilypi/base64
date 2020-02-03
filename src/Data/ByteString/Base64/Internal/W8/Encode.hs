{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.W8.Encode
-- Copyright 	: (c) 2019-2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- 'Word8' fallback loop
--
module Data.ByteString.Base64.Internal.W8.Encode
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
    -> Ptr Word16
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word8 -> IO ())
    -> IO ()
innerLoop etable sptr dptr end finish = go sptr dptr
  where
    go !src !dst
      | plusPtr src 2 >= end = finish src (castPtr dst)
      | otherwise = do

        !i <- w32 <$> peek src
        !j <- w32 <$> peek (plusPtr src 1)
        !k <- w32 <$> peek (plusPtr src 2)

        let !w = (shiftL i 16) .|. (shiftL j 8) .|. k

        !x <- peekElemOff etable (fromIntegral (shiftR w 12))
        !y <- peekElemOff etable (fromIntegral (w .&. 0xfff))

        poke dst x
        poke (plusPtr dst 2) y

        go (plusPtr src 3) (plusPtr dst 4)

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
innerLoopNopad etable sptr dptr end finish = go sptr dptr 0
  where
    go !src !dst !n
      | plusPtr src 2 >= end = finish src (castPtr dst) n
      | otherwise = do
        !i <- w32 <$> peek src
        !j <- w32 <$> peek (plusPtr src 1)
        !k <- w32 <$> peek (plusPtr src 2)

        let !w = (shiftL i 16) .|. (shiftL j 8) .|. k
        !x <- peekElemOff etable (fromIntegral (shiftR w 12))
        !y <- peekElemOff etable (fromIntegral (w .&. 0xfff))

        poke dst x
        poke (plusPtr dst 2) y

        go (plusPtr src 3) (plusPtr dst 4) (n + 4)
