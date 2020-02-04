{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.Head
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- Encoding header which initiates the encoding loop
--
module Data.ByteString.Base64.Internal.Head
( encodeBase64_
, encodeBase64Url_
, encodeBase64UrlNopad_
) where

#include "MachDeps.h"

import Data.ByteString.Internal
import Data.ByteString.Base64.Internal.Tables
import Data.ByteString.Base64.Internal.Tail
#if WORD_SIZE_IN_BITS == 32
import Data.ByteString.Base64.Internal.W32.Loop
#elif WORD_SIZE_IN_BITS >= 64
import Data.ByteString.Base64.Internal.W64.Loop
#else
import Data.ByteString.Base64.Internal.W8.Loop
#endif

import Foreign.ForeignPtr
import Foreign.Ptr

import GHC.Exts
import GHC.ForeignPtr

import System.IO.Unsafe

encodeBase64_ :: ByteString -> ByteString
encodeBase64_ (PS !sfp !soff !slen) =
    unsafeCreate dlen $ \dptr ->
      withForeignPtr sfp $ \sptr -> do
      let !end = plusPtr sptr (soff + slen)
#if WORD_SIZE_IN_BITS < 32
      withForeignPtr efp $ \eptr -> do
        innerLoop eptr (plusPtr sptr soff) (castPtr dptr) end (loopTail aptr end)
  where
    !dlen = 4 * ((slen + 2) `div` 3)
    EncodingTable !aptr !efp = base64Table
#else
      innerLoop
        c_enc_table_12bit_std
        (plusPtr sptr soff)
        (castPtr dptr)
        end
        (loopTail aptr end)
  where
    !aptr = Ptr "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#
    !dlen = 4 * ((slen + 2) `div` 3)
#endif

encodeBase64Url_ :: ByteString -> ByteString
encodeBase64Url_ (PS !sfp !soff !slen) =
    unsafeCreate dlen $ \dptr ->
      withForeignPtr sfp $ \sptr -> do
      let !end = plusPtr sptr (soff + slen)
#if WORD_SIZE_IN_BITS < 32
      withForeignPtr efp $ \eptr -> do
        innerLoop eptr (plusPtr sptr soff) (castPtr dptr) end (loopTail aptr end)
  where
    !dlen = 4 * ((slen + 2) `div` 3)
    EncodingTable !aptr !efp = base64UrlTable
#else
      innerLoop
        c_enc_table_12bit_url
        (plusPtr sptr soff)
        (castPtr dptr)
        end
        (loopTail aptr end)
#endif
  where
    !aptr = Ptr "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"#
    !dlen = 4 * ((slen + 2) `div` 3)

encodeBase64UrlNopad_ :: ByteString -> ByteString
encodeBase64UrlNopad_ (PS !sfp !soff !slen) = unsafeDupablePerformIO $ do
    dfp <- mallocPlainForeignPtrBytes dlen
    withForeignPtr dfp $ \dptr ->
      withForeignPtr sfp $ \sptr -> do
      let !end = plusPtr sptr (soff + slen)
#if WORD_SIZE_IN_BITS < 32
      withForeignPtr efp $ \eptr -> do
        innerLoop eptr (plusPtr sptr soff) (castPtr dptr) end (loopTail dfp aptr end)
  where
    !dlen = 4 * ((slen + 2) `div` 3)
    EncodingTable !aptr !efp = base64UrlTable
#else
      innerLoopNopad
        c_enc_table_12bit_url
        (plusPtr sptr soff)
        (castPtr dptr)
        end
        (loopTailNoPad dfp aptr end)
#endif
  where
    !aptr = Ptr "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"#
    !dlen = 4 * ((slen + 2) `div` 3)
