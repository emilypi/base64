{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.ByteString.Base64.Internal
( -- * Base64 encoding
  encodeB64Padded
, encodeB64Unpadded

  -- * Encoding Tables
  -- ** Standard
, base64Table

  -- ** Base64-URL
, base64UrlTable
) where


import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Exts
import GHC.Word


-- -------------------------------------------------------------------------- --
-- Internal data

-- | Only the lookup table need be a foreignptr,
-- and then, only so that we can automate some touches to keep it alive
--
data T2 = T2
  {-# UNPACK #-} !(Ptr Word8)
  {-# UNPACK #-} !(ForeignPtr Word16)

packTable :: Addr# -> T2
packTable alphabet = T2 (Ptr alphabet) (castForeignPtr efp)
  where
    (PS efp _ _) = BS.pack . concat $
      [ [ ix i, ix j ]
        | !i <- [0..63]
        , !j <- [0..63]
      ]
    ix (I# n) = W8# (indexWord8OffAddr# alphabet n)
{-# INLINE packTable #-}

base64UrlTable :: T2
base64UrlTable = packTable "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"#
{-# NOINLINE base64UrlTable #-}

base64Table :: T2
base64Table = packTable "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#
{-# NOINLINE base64Table #-}

-- -------------------------------------------------------------------------- --
-- Unpadded Base64

encodeB64Unpadded :: T2 -> ByteString -> ByteString
encodeB64Unpadded (T2 _ !efp) (PS sfp !soff !slen) =
    unsafeCreate dlen $ \dptr ->
    withForeignPtr sfp $ \sptr ->
    withForeignPtr efp $ \eptr ->
      encodeB64UnpaddedInternal
        eptr
        sptr
        (castPtr dptr)
        (sptr `plusPtr` (soff + slen))
  where
    dlen :: Int
    !dlen = 4 * ((slen + 2) `div` 3)
{-# INLINE encodeB64Unpadded #-}

-- | Unpadded Base64. The implicit assumption is that the input
-- data has a length that is a multiple of 3
--
encodeB64UnpaddedInternal
    :: Ptr Word16
    -> Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> IO ()
encodeB64UnpaddedInternal etable sptr dptr end = go sptr dptr
  where
    _eq = 0x3d :: Word8

    w32 :: Word8 -> Word32
    w32 i = fromIntegral i

    go !src !dst
      | src >= end = return ()
      | otherwise = do

        -- ideally, we want to read @uint32_t w = src[0..3]@ and simply
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

        go (src `plusPtr` 3) (dst `plusPtr` 4)
{-# INLINE encodeB64UnpaddedInternal #-}

-- -------------------------------------------------------------------------- --
-- Padded Base64

encodeB64Padded :: T2 -> ByteString -> ByteString
encodeB64Padded (T2 !aptr !efp) (PS !sfp !soff !slen) =
    unsafeCreate dlen $ \dptr ->
    withForeignPtr sfp $ \sptr ->
    withForeignPtr efp $ \eptr ->
      encodeB64PaddedInternal
        aptr
        eptr
        sptr
        (castPtr dptr)
        (sptr `plusPtr` (soff + slen))
  where
    dlen :: Int
    !dlen = 4 * ((slen + 2) `div` 3)
{-# INLINE encodeB64Padded #-}

encodeB64PaddedInternal
    :: Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> IO ()
encodeB64PaddedInternal (Ptr !alpha) !etable !sptr !dptr !end = go sptr dptr
  where
    ix (W8# !i) = W8# (indexWord8OffAddr# alpha (word2Int# i))

    w32 :: Word8 -> Word32
    w32 i = fromIntegral i
    {-# INLINE w32 #-}

    go !src !dst
      | plusPtr src 2 >= end = finalize src (castPtr dst)
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

          pokeByteOff dst 1 (ix b')
          pokeByteOff dst 2 (ix c')
        else do
          pokeByteOff dst 1 (ix b)
          pokeByteOff @Word8 dst 2 0x3d

        pokeByteOff @Word8 dst 3 0x3d
{-# INLINE encodeB64PaddedInternal #-}
