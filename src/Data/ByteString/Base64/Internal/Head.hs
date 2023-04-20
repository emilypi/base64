{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Data.Base64.Types.Internal
import Data.ByteString.Base64.Internal.Tail
import Data.ByteString.Base64.Internal.Utils
import Data.ByteString.Base64.Internal.W64.Loop
import Data.ByteString.Internal
import Data.Text (Text)

import Foreign.ForeignPtr
import Foreign.Ptr

import GHC.ForeignPtr
import GHC.Word

import System.IO.Unsafe ( unsafeDupablePerformIO )

#ifdef SIMD
import Foreign.C.Types (CChar, CInt, CSize)
import Foreign.Storable (peek)
import qualified Foreign.Marshal.Utils as Foreign
import qualified Data.Text as T
import LibBase64Bindings
#endif

encodeBase64_ :: EncodingTable -> ByteString -> ByteString
#ifdef SIMD
encodeBase64_ table b@(PS _ _ !slen)
  | slen < threshold = encodeBase64Loop_ table b
  | otherwise        = encodeBase64Simd_ b
  where
    !threshold = 1000 -- 1k
#else
encodeBase64_ table b = encodeBase64Loop_ table b
#endif
{-# inline encodeBase64_ #-}

#ifdef SIMD
encodeBase64Simd_ :: ByteString -> ByteString
encodeBase64Simd_ (PS !sfp !soff !slen) =
  unsafeDupablePerformIO $ do
    dfp <- mallocPlainForeignPtrBytes dlen
    dlenFinal <- do
      withForeignPtr dfp $ \out ->
        withForeignPtr sfp $ \src -> do
          Foreign.with (intToCSize dlen) $ \outlen -> do
            base64_encode
              (plusPtr (castPtr src :: Ptr CChar) soff)
              (intToCSize slen)
              out
              outlen
              base64Flags
            peek outlen
    pure (PS (castForeignPtr dfp) 0 (cSizeToInt dlenFinal))
  where
    !dlen = 4 * ((slen + 2) `div` 3)
    !base64Flags = 0
#endif

encodeBase64Loop_ :: EncodingTable -> ByteString -> ByteString
encodeBase64Loop_ (EncodingTable !aptr !efp) (PS !sfp !soff !slen) =
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
            (loopTail dfp dptr aptr (castPtr end))
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

#ifdef SIMD
decodeBase64Simd_ :: ByteString -> IO (Either Text ByteString)
decodeBase64Simd_ (PS !sfp !soff !slen) = do
  withForeignPtr sfp $ \src -> do
    dfp <- mallocPlainForeignPtrBytes dlen
    edlenFinal :: Either Text CSize <- do
      withForeignPtr dfp $ \out -> do
        Foreign.with (intToCSize dlen) $ \outlen -> do
          decodeResult <- base64_decode
            (plusPtr (castPtr src :: Ptr CChar) soff)
            (intToCSize slen)
            out
            outlen
            base64Flags
          case decodeResult of
            1    -> Right <$> peek outlen
            0    -> pure (Left "SIMD: Invalid input")
            (-1) -> pure (Left "Invalid Codec")
            x    -> pure (Left ("Unexpected result from libbase64 base64_decode: " <> T.pack (show (cIntToInt x))))
    pure $ fmap
      (\dlenFinal -> PS (castForeignPtr dfp) 0 (cSizeToInt dlenFinal))
      edlenFinal
  where
    !dlen = (slen `quot` 4) * 3
    !base64Flags = 0
#endif

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
#ifdef SIMD
decodeBase64_ dtfp b@(PS _ _ !slen)
  | slen < threshold = decodeBase64Loop_ dtfp b
  | otherwise        = decodeBase64Simd_ b
  where
    !threshold = 250
#else
decodeBase64_ dtfp b = decodeBase64Loop_ dtfp b
#endif
{-# inline decodeBase64_ #-}

decodeBase64Loop_
    :: ForeignPtr Word8
    -> ByteString
    -> IO (Either Text ByteString)
decodeBase64Loop_ !dtfp (PS !sfp !soff !slen) =
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
{-# inline decodeBase64Loop_ #-}

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
    -> ByteString
decodeBase64Typed_ !dtfp (Base64 (PS sfp soff slen))
  | slen == 0 = mempty
  | otherwise = unsafeDupablePerformIO $
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

#ifdef SIMD
intToCSize :: Int -> CSize
intToCSize = fromIntegral

cSizeToInt :: CSize -> Int
cSizeToInt = fromIntegral

cIntToInt :: CInt -> Int
cIntToInt = fromIntegral
#endif
