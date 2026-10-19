{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.SIMD
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Pointer callers retain buffers with @withForeignPtr@. Byte-array callers
-- pass arrays directly to unsafe FFI calls, during which GHC cannot run GC;
-- neither pinning nor copying is needed. The kernels retain no pointers and
-- make no Haskell callbacks. The decoder leaves the
-- final quanta and stops before invalid blocks so scalar code retains all
-- padding checks and error reporting, with canonicity checks *preserved*. 
--
-- Why not do something like Alfred Klomp's `fastbase64`? Because we can do better!
module Data.ByteString.Base64.Internal.SIMD
( encodePrefix
, decodePrefix
, encodeByteArrayPrefix
, decodeByteArrayPrefix
, available
) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Primitive.ByteArray (ByteArray(..), MutableByteArray(..))
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import GHC.Exts (ByteArray#, MutableByteArray#)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "hs_base64_simd_backend"
  c_backend :: IO CInt

-- | Whether this process can use a native vector backend. CPU/OS capability
-- is stable for the lifetime of the process, so cache the dispatch result.
-- Short-array callers use the established scalar loops on unsupported CPUs.
available :: Bool
available = unsafePerformIO ((/= 0) <$> c_backend)
{-# NOINLINE available #-}

foreign import ccall unsafe "hs_base64_simd_encode"
  c_encodePrefix :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr Word8 -> IO CSize

foreign import ccall unsafe "hs_base64_simd_decode"
  c_decodePrefix :: Ptr Word8 -> CSize -> Ptr Word8 -> Ptr Word8 -> IO CSize

foreign import ccall unsafe "hs_base64_simd_encode"
  c_encodeByteArrayPrefix
    :: ByteArray# -> CSize -> MutableByteArray# s -> Ptr Word8 -> IO CSize

foreign import ccall unsafe "hs_base64_simd_decode"
  c_decodeByteArrayPrefix
    :: ByteArray# -> CSize -> MutableByteArray# s -> Ptr Word8 -> IO CSize

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

-- | Encode complete triples into a distinct mutable destination. The caller
-- provides room for four bytes per triple and retains the alphabet pointer.
-- The foreign call reads the source and writes only the destination, so it
-- can be embedded in ST without letting mutable state escape.
encodeByteArrayPrefix
  :: ByteArray -> Int -> MutableByteArray s -> Ptr Word8 -> ST s Int
encodeByteArrayPrefix (ByteArray src) len (MutableByteArray dst) alphabet
  | len < 96 = pure 0
  | otherwise = unsafeIOToST $
      fromIntegral <$> c_encodeByteArrayPrefix src (fromIntegral len) dst alphabet
{-# INLINE encodeByteArrayPrefix #-}

-- | Decode complete valid quartets into a distinct mutable destination with
-- room for three bytes per quartet. Retain the decode table across the call.
-- The final quartet remains for scalar padding and canonical-bit checks.
decodeByteArrayPrefix
  :: ByteArray -> Int -> MutableByteArray s -> Ptr Word8 -> ST s Int
decodeByteArrayPrefix (ByteArray src) len (MutableByteArray dst) table
  | len < 128 = pure 0
  | otherwise = unsafeIOToST $
      fromIntegral <$> c_decodeByteArrayPrefix src (fromIntegral len) dst table
{-# INLINE decodeByteArrayPrefix #-}
