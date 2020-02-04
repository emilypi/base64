{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base64.Internal
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- Internal module defining the encoding and decoding
-- processes and tables.
--
module Data.ByteString.Base64.Internal
( encodeBase64_
, encodeBase64Nopad_
, decodeBase64_
, decodeBase64Lenient_
, validateBase64
) where


#include "MachDeps.h"

import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Base64.Internal.Tail
import Data.ByteString.Base64.Internal.Types
import Data.ByteString.Base64.Internal.Utils
#if WORD_SIZE_IN_BITS == 32
import Data.ByteString.Base64.Internal.W32.Encode
#elif WORD_SIZE_IN_BITS == 64
import Data.ByteString.Base64.Internal.W64.Encode
#else
import Data.ByteString.Base64.Internal.W8.Encode
#endif

import Data.ByteString.Internal
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Exts
import GHC.ForeignPtr
import GHC.Word

import System.IO.Unsafe


-- -------------------------------------------------------------------------- --
-- Validating Base64

-- | Given a bytestring, check to see that it conforms to a given alphabet
--
validateBase64 :: ByteString -> ByteString -> Bool
validateBase64 !alphabet (PS fp off l) =
    accursedUnutterablePerformIO $ withForeignPtr fp $ \p ->
      go (plusPtr p off) (plusPtr p (l + off))
  where
    go !p !end
      | p == end = return True
      | otherwise = do
        w <- peek p

        let f a
              | a == 0x3d, plusPtr p 1 == end = True
              | a == 0x3d, plusPtr p 2 == end = True
              | a == 0x3d = False
              | otherwise = BS.elem a alphabet

        if f w then go (plusPtr p 1) end else return False
{-# INLINE validateBase64 #-}

-- -------------------------------------------------------------------------- --
-- Encode Base64

encodeBase64_ :: EncodingTable -> ByteString -> ByteString
encodeBase64_ (EncodingTable !aptr !efp) (PS !sfp !soff !slen) =
    unsafeCreate dlen $ \dptr ->
    withForeignPtr sfp $ \sptr ->
    withForeignPtr efp $ \eptr -> do
      let !end = plusPtr sptr (soff + slen)
      innerLoop
        eptr
        (plusPtr sptr soff)
        (castPtr dptr)
        end
        (loopTail aptr end)
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
          innerLoopNopad
            etable
            (plusPtr sptr soff)
            (castPtr dptr)
            end
            (loopTailNoPad dfp aptr end)
  where
    !dlen = 4 * ((slen + 2) `div` 3)


-- -------------------------------------------------------------------------- --
-- Decoding Base64

-- | The main decode function. Takes a padding flag, a decoding table, and
-- the input value, producing either an error string on the left, or a
-- decoded value.
--
-- Note: If 'Padding' ~ Pad, then we pad out the input to a multiple of 4.
-- If 'Padding' ~ NoPad, then we do not, and fail if the input is not
-- a multiple of 4 in length.
--
decodeBase64_ :: Padding -> ForeignPtr Word8 -> ByteString -> Either Text ByteString
decodeBase64_ pad !dtfp bs@(PS _ _ !slen) = case pad of
    Pad -> go (BS.append bs (BS.replicate r 0x3d))
    NoPad
      | r /= 0 -> Left "invalid padding"
      | otherwise -> go bs
  where
    (!q, !r) = divMod slen 4
    !dlen = q * 3

    go (PS !sfp !soff !slen') = unsafeDupablePerformIO $
      withForeignPtr dtfp $ \dtable ->
        withForeignPtr sfp $ \sptr -> do
        dfp <- mallocPlainForeignPtrBytes dlen
        withForeignPtr dfp $ \dptr ->
          decodeBase64_'
            dtable
            (plusPtr sptr soff)
            dptr
            (plusPtr sptr (soff + slen'))
            dfp
{-# INLINE decodeBase64_ #-}

decodeBase64_'
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
    -> IO (Either Text ByteString)
decodeBase64_' !dtable !sptr !dptr !end !dfp = go dptr sptr 0
  where
    err = return . Left . T.pack
    {-# INLINE err #-}

    finalize !n = return (Right (PS dfp 0 n))
    {-# INLINE finalize #-}

    look :: Ptr Word8 -> IO Word32
    look p = do
      !i <- peekByteOff @Word8 p 0
      !v <- peekByteOff @Word8 dtable (fromIntegral i)
      return (fromIntegral v)

    go !dst !src !n
      | src >= end = return (Right (PS dfp 0 n))
      | otherwise = do
        a <- look src
        b <- look (src `plusPtr` 1)
        c <- look (src `plusPtr` 2)
        d <- look (src `plusPtr` 3)

        if a == 0x63 || b == 0x63
        then err
          $ "invalid padding near offset: "
          ++ show (minusPtr src sptr)
        else
          if a .|. b .|. c .|. d == 0xff
          then err
            $ "invalid base64 encoding near offset: "
            ++ show (minusPtr src sptr)
          else do
            let !w = (shiftL a 18) .|. (shiftL b 12) .|. (shiftL c 6) .|. d

            poke @Word8 dst (fromIntegral (shiftR w 16))
            if c == 0x63
            then finalize (n + 1)
            else do
              poke @Word8 (plusPtr dst 1) (fromIntegral (shiftR w 8))
              if d == 0x63
              then finalize (n + 2)
              else do
                poke @Word8 (plusPtr dst 2) (fromIntegral w)
                go (plusPtr dst 3) (plusPtr src 4) (n + 3)
{-# INLINE decodeBase64_' #-}


decodeBase64Lenient_ :: ForeignPtr Word8 -> ByteString -> ByteString
decodeBase64Lenient_ !dtfp (PS !sfp !soff !slen) = unsafeDupablePerformIO $
    withForeignPtr dtfp $ \dtable ->
    withForeignPtr sfp $ \sptr -> do
      dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dptr ->
        decodeBase64Lenient_'
          dtable
          (plusPtr sptr soff)
          dptr
          (plusPtr sptr (soff + slen))
          dfp
  where
    !dlen = ((slen + 3) `div` 4) * 3
{-# INLINE decodeBase64Lenient_ #-}


decodeBase64Lenient_'
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
decodeBase64Lenient_' !dtable !sptr !dptr !end !dfp = go dptr sptr 0
  where
    finalize !n = return (PS dfp 0 n)
    {-# INLINE finalize #-}

    look skip !p_ f = k p_
      where
        k !p
          | p >= end = f (plusPtr end (-1)) (0x63 :: Word32)
          | otherwise = do
            !i <- peekByteOff @Word8 p 0
            !v <- peekByteOff @Word8 dtable (fromIntegral i)

            if
              | v == 0xff -> k (plusPtr p 1)
              | v == 0x63, skip -> k (plusPtr p 1)
              | otherwise -> f (plusPtr p 1) (fromIntegral v)

    go !dst !src !n
      | src >= end = finalize n
      | otherwise =
        look True src $ \ap a ->
        look True ap $ \bp b ->
          if
            | a == 0x63 -> finalize n
            | b == 0x63 -> finalize n
            | otherwise ->
              look False bp $ \cp c ->
              look False cp $ \dp d -> do
                let !w = (shiftL a 18) .|. (shiftL b 12) .|. (shiftL c 6) .|. d

                poke @Word8 dst (fromIntegral (shiftR w 16))
                if c == 0x63
                then finalize (n + 1)
                else do
                  poke @Word8 (plusPtr dst 1) (fromIntegral (w `shiftR` 8))
                  if d == 0x63
                  then finalize (n + 2)
                  else do
                    poke @Word8 (plusPtr dst 2) (fromIntegral w)
                    go (plusPtr dst 3) dp (n + 3)
{-# INLINE decodeBase64Lenient_' #-}
