{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.ByteString.Base64.Internal
( base64Padded
, base64Unpadded
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



base64Padded :: T2 -> ByteString -> ByteString
base64Padded (T2 !aptr !efp) (PS sfp !soff !slen) =
    unsafeCreate dlen $ \dptr ->
    withForeignPtr sfp $ \sptr ->
    withForeignPtr efp $ \eptr ->
      base64InternalPadded aptr eptr sptr (castPtr dptr) (sptr `plusPtr` (soff + slen))
  where
    dlen :: Int
    !dlen = 4 * ((slen + 2) `div` 3) + 1

base64Unpadded :: T2 -> ByteString -> ByteString
base64Unpadded (T2 !aptr !efp) (PS sfp !soff !slen) =
    unsafeCreate dlen $ \dptr ->
    withForeignPtr sfp $ \sptr ->
    withForeignPtr efp $ \eptr ->
      base64InternalUnpadded aptr eptr sptr (castPtr dptr) (sptr `plusPtr` (soff + slen))
  where
    dlen :: Int
    !dlen = 4 * ((slen + 2) `div` 3)

-- | Only the lookup table need be a foreignptr,
-- and then, only so that we can automate some touches to keep it alive
--
data T2 = T2
  {-# UNPACK #-} !(Ptr Word8)
  {-# UNPACK #-} !(ForeignPtr Word16)

base64Table :: T2
base64Table = packTable "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#
{-# NOINLINE base64Table #-}

base64UrlTable :: T2
base64UrlTable = packTable "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"#
{-# NOINLINE base64UrlTable #-}

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

-- | Unpadded Base64
--
base64InternalUnpadded
    :: Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> IO ()
base64InternalUnpadded alpha etable sptr dptr end = go sptr dptr
  where
    ix :: Int -> IO Word8
    ix n = peek (plusPtr alpha n)

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
{-# INLINE base64InternalUnpadded #-}

base64InternalPadded
    :: Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> IO ()
base64InternalPadded alpha etable sptr dptr end = go sptr dptr
  where
    ix :: Int -> IO Word8
    ix n = peek (plusPtr alpha n)

    w32 :: Word8 -> Word32
    w32 i = fromIntegral i
    {-# INLINE w32 #-}

    go !src !dst
      | src `plusPtr` 2 >= end = finalize src (castPtr dst)
      | otherwise = do
        !i <- w32 <$> peek src
        !j <- w32 <$> peek (src `plusPtr` 1)
        !k <- w32 <$> peek (src `plusPtr` 2)

        let !w = (i `shiftL` 16) .|. (j `shiftL` 8) .|. k

        !x <- peekElemOff etable (fromIntegral (shiftR @Word32 w 12))
        !y <- peekElemOff etable (fromIntegral (w .&. 0xfff))

        poke dst x
        poke (plusPtr dst 2) y

        go (src `plusPtr` 3) (dst `plusPtr` 4)


    finalize :: Ptr Word8 -> Ptr Word8 -> IO ()
    finalize !src !dst
      | src == end = return ()
      | otherwise = do
        let peekSP n f = f . fromIntegral <$> peek @Word8 (src `plusPtr` n)
            !twoMore = src `plusPtr` 2 == end

        !a <- peekSP 0 ((`shiftR` 2) . (.&. 0xfc))
        !b <- peekSP 0 ((`shiftL` 4) . (.&. 0x03))

        poke dst =<< ix a

        if twoMore
        then do
          b' <- peekSP 1 ((.|. b) . (`shiftR` 4) . (.&. 0xf0))
          poke (plusPtr dst 1) =<< ix b'

          c <- ix =<< peekSP 1 ((`shiftL` 2) . (.&. 0x0f))
          poke (dst `plusPtr` 2) c
        else do
          poke (plusPtr dst 1) =<< ix b
          poke @Word8 (dst `plusPtr` 2) 0x3d

        poke @Word8 (dst `plusPtr` 3) 0x3d
{-# INLINE base64InternalPadded #-}
