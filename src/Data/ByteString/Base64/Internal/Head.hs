{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.Head
-- Copyright    : (c) 2019-2022 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Shared internal utils
--
module Data.ByteString.Base64.Internal.Head
( encodeBase64_
, encodeBase64Nopad_
, decodeBase64_
, decodeBase64Typed_
, decodeBase64Lenient_
) where

#include "MachDeps.h"

import Data.Base64.Types.Internal
import Data.ByteString.Base64.Internal.Tail
import Data.ByteString.Base64.Internal.Utils
#if WORD_SIZE_IN_BITS == 32
import Data.ByteString.Base64.Internal.W32.Loop
#elif WORD_SIZE_IN_BITS >= 64
import Data.ByteString.Base64.Internal.W64.Loop
#else
import Data.ByteString.Base64.Internal.W16.Loop
#endif
import Data.ByteString.Internal
import Data.Text (Text)

import Foreign.ForeignPtr
import Foreign.Ptr

import GHC.ForeignPtr
import GHC.Word

import System.IO.Unsafe


encodeBase64_ :: EncodingTable -> ByteString -> ByteString
encodeBase64_ (EncodingTable !aptr !efp) (PS !sfp !soff !slen) =
    unsafeDupablePerformIO $ do
      dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr ->
        withForeignPtr sfp $ \sptr ->
        withForeignPtr efp $ \eptr -> do
          let !end = plusPtr sptr (soff + slen)
          innerLoop
            eptr
            (castPtr (plusPtr sptr soff))
            (castPtr dptr)
            end
            (loopTail dfp aptr dptr (castPtr end))
  where
    !dlen = 4 * ((slen + 2) `div` 3)

encodeBase64Nopad_ :: EncodingTable -> ByteString -> ByteString
encodeBase64Nopad_ (EncodingTable !aptr !efp) (PS !sfp !soff !slen) =
    unsafeDupablePerformIO $ do
      dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr ->
        withForeignPtr efp $ \etable ->
        withForeignPtr sfp $ \sptr -> do
          let !end = plusPtr sptr (soff + slen)
          innerLoop
            etable
            (castPtr (plusPtr sptr soff))
            (castPtr dptr)
            end
            (loopTailNoPad dfp aptr dptr (castPtr end))
  where
    !dlen = 4 * ((slen + 2) `div` 3)

-- | The main decode function. Takes a padding flag, a decoding table, and
-- the input value, producing either an error string on the left, or a
-- decoded value.
--
-- Note: If 'Padding' ~ 'Don\'tCare', then we pad out the input to a multiple of 4.
-- If 'Padding' ~ 'Padded', then we do not, and fail if the input is not
-- a multiple of 4 in length. If 'Padding' ~ 'Unpadded', then we validate
-- correctness of length and the absence of padding and then treat as a std
-- padded string.
--
decodeBase64_
    :: ForeignPtr Word8
    -> ByteString
    -> IO (Either Text ByteString)
decodeBase64_ !dtfp (PS !sfp !soff !slen) =
    withForeignPtr dtfp $ \dtable ->
    withForeignPtr sfp $ \sptr -> do
      dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr -> do
        let !end = plusPtr sptr (soff + slen)
        decodeLoop dtable
          (plusPtr sptr soff)
          dptr end dfp
  where
    !dlen = (slen `quot` 4) * 3
{-# inline decodeBase64_ #-}

-- | The main decode function for typed base64 values.
--
-- This loop is separate from 'decodeBase64_' due to the fact that
-- when taking a 'Base64' value from this library, the existence
-- of the wrapper is a witness to the well-formedness of the underlying value,
-- and so we can eschew error checking in the decode loop.
--
decodeBase64Typed_
    :: ForeignPtr Word8
    -> Base64 k ByteString
    -> IO (Either Text ByteString)
decodeBase64Typed_ !dtfp (Base64 (PS !sfp !soff !slen)) =
    withForeignPtr dtfp $ \dtable ->
    withForeignPtr sfp $ \sptr -> do
      dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr -> do
        let !end = plusPtr sptr (soff + slen)
        decodeLoopNoError dtable
          (plusPtr sptr soff)
          dptr end dfp
  where
    !dlen = (slen `quot` 4) * 3
{-# inline decodeBase64Typed_ #-}

decodeBase64Lenient_ :: ForeignPtr Word8 -> ByteString -> ByteString
decodeBase64Lenient_ !dtfp (PS !sfp !soff !slen) = unsafeDupablePerformIO $
    withForeignPtr dtfp $ \dtable ->
    withForeignPtr sfp $ \sptr -> do
      dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr ->
        lenientLoop
          dtable
          (plusPtr sptr soff)
          dptr
          (plusPtr sptr (soff + slen))
          dfp
  where
    !dlen = ((slen + 3) `div` 4) * 3
