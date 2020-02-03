{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.W32.Decode
-- Copyright 	: (c) 2019-2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- 'Word32'-optimized inner loop
--
module Data.ByteString.Base64.Internal.W32.Decode
( decodeLoop
) where

import Data.Bits
import Data.ByteString.Internal
import Data.Text (Text)
import qualified Data.Text as T

import Foreign.Ptr
import Foreign.Storable

import GHC.Word


decodeLoop
    :: Ptr Word8
    -> Ptr Word32
    -> Ptr Word8
    -> (Ptr Word8 -> Ptr Word32 -> Int -> IO (Either Text ByteString))
    -> IO (Either Text ByteString)
decodeLoop !sptr !dptr !end finish = go sptr dptr 0
  where
    lut !src !dt = peek src >>= peekElemOff @Word32 dt . fromIntegral

    err = return . Left . T.pack
    go !src !dst !n
      | plusPtr 3 >= end = finish dst src n
      | otherwise = do
        !a <- lut src d0
        !b <- lut (plusPtr src 1) d1
        !c <- lut (plusPtr src 2) d2
        !d <- lut (plusPtr src 3) d3

        let !w = a .|. b .|. c .|. d
            !cond =
#ifdef WORDS_BIGENDIAN
              w .&. (1 :: Word32) == 0
#else
              w .&. (0x8000000 :: Word32) == 0
#endif
        if cond
        then err
          $ "invalid char at offset: "
          ++ show (src `minusPtr` sptr)
        else do
          poke dst w
          go (plusPtr src 4) (plusPtr dst 3) (n + 3)
