{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.Head
-- Copyright    : (c) 2019-2020 Emily Pillmore
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
, decodeBase64Lenient_
) where

#include "MachDeps.h"

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

import GHC.Word

import System.IO.Unsafe


encodeBase64_ :: EncodingTable -> ByteString -> ByteString
encodeBase64_ (EncodingTable !aptr !efp) (PS !sfp !soff !slen) =
    unsafeDupablePerformIO $ do
      dfp <- mallocByteString dlen
      withForeignPtr dfp $ \dptr ->
        withForeignPtr sfp $ \sptr ->
        withForeignPtr efp $ \eptr -> do
          let !end = plusPtr sptr (slen + soff)
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
      dfp <- mallocByteString dlen
      withForeignPtr dfp $ \dptr ->
        withForeignPtr efp $ \etable ->
        withForeignPtr sfp $ \sptr -> do
          let end = plusPtr sptr (slen + soff)
          innerLoop
            etable (castPtr (plusPtr sptr soff)) (castPtr dptr) end
            (loopTailNoPad dfp aptr dptr (castPtr end))
  where
    !dlen = 4 * ((slen + 2) `div` 3)

decodeBase64_
    :: Int
    -> ForeignPtr Word8
    -> ByteString
    -> IO (Either Text ByteString)
decodeBase64_ !dlen !dtfp (PS !sfp !soff !slen) = do
    withForeignPtr sfp $ \sptr -> do
      dfp <- mallocByteString dlen
      withForeignPtr dtfp $ \ !dtable ->
        withForeignPtr dfp $ \dptr -> decodeLoop
          dtable (plusPtr sptr soff)
          dptr (plusPtr sptr (slen + soff)) dfp
{-# inline decodeBase64_ #-}

decodeBase64Lenient_ :: ForeignPtr Word8 -> ByteString -> ByteString
decodeBase64Lenient_ !dtfp (PS !sfp !soff !slen) = unsafeDupablePerformIO $
    withForeignPtr dtfp $ \dtable ->
    withForeignPtr sfp $ \sptr -> do
      dfp <- mallocByteString dlen
      withForeignPtr dfp $ \dptr -> lenientLoop
        dtable (plusPtr sptr soff) dptr
        (plusPtr sptr (slen + soff)) dfp
  where
    !dlen = ((slen + 3) `div` 4) * 3
