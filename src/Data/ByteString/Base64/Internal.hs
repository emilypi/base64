{-# LANGUAGE BangPatterns #-}
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
( -- * Base64 encoding
  encodeBase64_

  -- * Base64 decoding
, decodeBase64_
, decodeBase64Lenient_

  -- * Decoding Tables
  -- ** Standard
, decodeB64Table
  -- ** Base64-url
, decodeB64UrlTable

  -- * Encoding Tables
  -- ** Standard
, base64Table

  -- ** Base64-url
, base64UrlTable
) where


import Control.Monad (when)

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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
-- Internal data

-- | Only the lookup table need be a foreignptr,
-- and then, only so that we can automate some touches to keep it alive
--
data EncodingTable = EncodingTable
  {-# UNPACK #-} !(Ptr Word8)
  {-# UNPACK #-} !(ForeignPtr Word16)

-- | Allocate and fill @n@ bytes with some data
--
writeNPlainForeignPtrBytes
    :: ( Storable a
       , Storable b
       )
    => Int
    -> [a]
    -> ForeignPtr b
writeNPlainForeignPtrBytes !n as = unsafeDupablePerformIO $ do
    fp <- mallocPlainForeignPtrBytes n
    withForeignPtr fp $ \p -> go p as
    return (castForeignPtr fp)
  where
    go !_ [] = return ()
    go !p (x:xs) = poke p x >> go (plusPtr p 1) xs

packTable :: Addr# -> EncodingTable
packTable alphabet = etable
  where
    ix (I# n) = W8# (indexWord8OffAddr# alphabet n)

    !etable =
      let bs = concat
            [ [ ix i, ix j ]
            | !i <- [0..63]
            , !j <- [0..63]
            ]
      in EncodingTable (Ptr alphabet) (writeNPlainForeignPtrBytes 8192 bs)

base64UrlTable :: EncodingTable
base64UrlTable = packTable "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"#
{-# NOINLINE base64UrlTable #-}

base64Table :: EncodingTable
base64Table = packTable "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#
{-# NOINLINE base64Table #-}


-- -------------------------------------------------------------------------- --
-- Encode Base64

encodeBase64_ :: Bool -> EncodingTable -> ByteString -> ByteString
encodeBase64_ padding (EncodingTable !aptr !efp) (PS !sfp !soff !slen) =
    unsafeCreate dlen $ \dptr ->
    withForeignPtr sfp $ \sptr ->
    withForeignPtr efp $ \eptr ->
      encodeBase64_'
        padding
        aptr
        eptr
        (plusPtr sptr soff)
        (castPtr dptr)
        (plusPtr sptr (soff + slen))
  where
    dlen :: Int
    !dlen = 4 * ((slen + 2) `div` 3)
{-# INLINE encodeBase64_ #-}

encodeBase64_'
    :: Bool
    -> Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> IO ()
encodeBase64_' !padded (Ptr !alpha) !etable !sptr !dptr !end = go sptr dptr
  where
    ix (W8# i) = W8# (indexWord8OffAddr# alpha (word2Int# i))

    w32 :: Word8 -> Word32
    w32 = fromIntegral

    go !src !dst
      | src >= end = return ()
      | plusPtr src 2 >= end
      , padded = finalize src (castPtr dst)
      | otherwise = do

        -- ideally, we want to do single read @uint32_t w = src[0..3]@ and simply
        -- discard the upper bits. TODO.
        --
        !i <- w32 <$> peek src
        !j <- w32 <$> peek (plusPtr src 1)
        !k <- w32 <$> peek (plusPtr src 2)

        -- pack 3 'Word8's into a the first 24 bits of a 'Word32'
        --
        let !w = (shiftL i 16) .|. (shiftL j 8) .|. k

        -- ideally, we'd want to pack this is in a single read, then
        -- a single write
        --
        !x <- peekElemOff etable (fromIntegral (shiftR w 12))
        !y <- peekElemOff etable (fromIntegral (w .&. 0xfff))

        poke dst x
        poke (plusPtr dst 2) y

        go (plusPtr src 3) (plusPtr dst 4)


    finalize :: Ptr Word8 -> Ptr Word8 -> IO ()
    finalize !src !dst
      | src == end = return ()
      | otherwise = do
        !k <- peekByteOff src 0

        let !a = shiftR (k .&. 0xfc) 2
            !b = shiftL (k .&. 0x03) 4

        pokeByteOff dst 0 (ix a)

        if plusPtr src 2 == end
        then do
          !k' <- peekByteOff src 1

          let !b' = shiftR (k' .&. 0xf0) 4 .|. b
              !c' = shiftL (k' .&. 0x0f) 2

          -- ideally, we'd want to pack these is in a single write
          --
          pokeByteOff dst 1 (ix b')
          pokeByteOff dst 2 (ix c')

          when padded (pokeByteOff @Word8 dst 3 0x3d)

        else do
          pokeByteOff dst 1 (ix b)
          when padded $ do
            pokeByteOff @Word8 dst 2 0x3d
            pokeByteOff @Word8 dst 3 0x3d
{-# INLINE encodeBase64_' #-}

-- -------------------------------------------------------------------------- --
-- Decoding Base64

-- | Non-URLsafe b64 decoding table (naive)
--
decodeB64Table :: ForeignPtr Word8
decodeB64Table = writeNPlainForeignPtrBytes @Word8 256
      [ 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x3e,0xff,0xff,0xff,0x3f
      , 0x34,0x35,0x36,0x37,0x38,0x39,0x3a,0x3b,0x3c,0x3d,0xff,0xff,0xff,0x63,0xff,0xff
      , 0xff,0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e
      , 0x0f,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0xff,0xff,0xff,0xff,0xff
      , 0xff,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f,0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28
      , 0x29,0x2a,0x2b,0x2c,0x2d,0x2e,0x2f,0x30,0x31,0x32,0x33,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      ]
{-# NOINLINE decodeB64Table #-}

decodeB64UrlTable :: ForeignPtr Word8
decodeB64UrlTable = writeNPlainForeignPtrBytes @Word8 256
      [ 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0x3e,0xff,0xff
      , 0x34,0x35,0x36,0x37,0x38,0x39,0x3a,0x3b,0x3c,0x3d,0xff,0xff,0xff,0x63,0xff,0xff
      , 0xff,0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e
      , 0x0f,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0xff,0xff,0xff,0xff,0x3f
      , 0xff,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f,0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28
      , 0x29,0x2a,0x2b,0x2c,0x2d,0x2e,0x2f,0x30,0x31,0x32,0x33,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
      ]
{-# NOINLINE decodeB64UrlTable #-}

decodeBase64_ :: Bool -> ForeignPtr Word8 -> ByteString -> Either Text ByteString
decodeBase64_ !padding !dtfp bs@(PS _ _ !slen)
    | padding =  go (BS.append bs (BS.replicate r 0x3d))
    | r /= 0 && (not padding) = Left "invalid padding"
    | otherwise = go bs
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

    finalize !n = return (Right (PS dfp 0 n))

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

    look
        :: Bool
        -> Ptr Word8
        -> (Ptr Word8 -> Word32 -> IO ByteString)
        -> IO ByteString
    look skip p_ f = k p_
      where
        k p
          | p >= end = f (plusPtr end (-1)) 0x63
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
