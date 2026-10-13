{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.SIMD
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Note: Buffers must remain alive for the duration
-- of each call, using @withForeignPtr@. The decoder leaves the
-- final quanta and stops before invalid blocks so scalar code retains all
-- padding checks and error reporting, with canonicity checks *preserved*. 
--
-- Why not do something like Alfred Klomp's `fastbase64`? Because we can do better!
module Data.ByteString.Base64.Internal.SIMD
( encodePrefix
, decodePrefix
) where

import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr)
import Data.Word (Word8)

foreign import ccall unsafe "hs_base64_simd_encode"
  c_encodePrefix :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr Word8 -> IO CSize

foreign import ccall unsafe "hs_base64_simd_decode"
  c_decodePrefix :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr Word8 -> IO CSize

-- | Encode a prefix; return the number of input bytes consumed (multiple of 3).
-- Short inputs avoid the foreign-call and runtime-dispatch overhead.
encodePrefix :: Ptr Word8 -> Int -> Ptr Word8 -> Ptr Word8 -> IO Int
encodePrefix src len dst alphabet
  | len < 96 = pure 0
  | otherwise = fromIntegral <$> c_encodePrefix src (fromIntegral len) dst alphabet
{-# INLINE encodePrefix #-}

-- | Decode a prefix; return the number of input bytes consumed (multiple of 4).
-- The original source and destination pointers are retained by the caller.
decodePrefix :: Ptr Word8 -> Int -> Ptr Word8 -> Ptr Word8 -> IO Int
decodePrefix src len dst table
  | len < 128 = pure 0
  | otherwise = fromIntegral <$> c_decodePrefix src (fromIntegral len) dst table
{-# INLINE decodePrefix #-}
