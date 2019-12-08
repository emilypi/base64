{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.ByteString.Base64.Internal
( base64Padded
) where


import Control.Monad (when)

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as LBS

import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Foreign.Storable

import GHC.Exts
import GHC.ForeignPtr
import GHC.Storable
import GHC.Word

import System.IO
import System.IO.Unsafe


-- bytestrings malloc plain frgn ptr bytes and so do not need
-- finalizers. The same ethos extends to alphabet addr's
--
base64Padded :: ByteString -> ByteString
base64Padded (PS (ForeignPtr !sptr _) !soff !slen) =
    unsafeCreate dlen $ \dptr ->
      base64InternalPadded aptr eptr src (castPtr dptr) (src `plusPtr` (soff + slen))
  where
    src :: Ptr Word8
    !src = Ptr sptr

    dlen :: Int
    !dlen = ((slen + 2) `div` 3) * 4

    T2 !aptr !eptr = base64ETable

data T2 = T2 !(Ptr Word8) !(Ptr Word16)

-- | We want this inlined so that new ptr's are packed per request.
--
base64ETable :: T2
base64ETable = T2 (Ptr alphabet) (Ptr eaddr)
  where
    (PS (ForeignPtr !eaddr _) _ _) = BS.pack $! base64Alphabet
    !alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#
{-# INLINE base64ETable #-}

-- | Don't inline this expensive boi
--
base64Alphabet :: [Word8]
base64Alphabet = concat $! [ [ix j, ix k] | !j <- [0..63], !k <- [0..63] ]
  where
    ix (I# !i) = W8# (indexWord8OffAddr# alphabet i)
    !alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#
{-# NOINLINE base64Alphabet #-}

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
    ix n = peek (alpha `plusPtr` n)

    _eq = 0x3d :: Word8

    w32 :: Word8 -> Word32
    w32 i = fromIntegral i

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
        let peekSP n f = (f . fromIntegral) `fmap` peek @Word8 (src `plusPtr` n)
            !twoMore = src `plusPtr` 2 == end

        !a <- peekSP 0 ((`shiftR` 2) . (.&. 0xfc))
        !b <- peekSP 0 ((`shiftL` 4) . (.&. 0x03))

        poke dst =<< ix a

        if twoMore
        then do
          b' <- peekSP 1 ((.|. b) . (`shiftR` 4) . (.&. 0xf0))
          poke (plusPtr dst 1) =<< ix b'
        else do
          poke (plusPtr dst 1) =<< ix b

        !c <- ix =<< peekSP 1 ((`shiftL` 2) . (.&. 0x0f))
        poke (dst `plusPtr` 2) c
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
        let peekSP n f = (f . fromIntegral) `fmap` peek @Word8 (src `plusPtr` n)
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

-- | Naive reference impl
--
base64InternalRef :: Addr# -> Ptr Word8 -> Ptr Word8 -> Int -> IO ()
base64InternalRef alpha src dst slen = go 0 0
  where
    _eq = 0x3d :: Word8

    go !i !j
      | i  >= slen = return ()
      | otherwise = do
        !a <- peekByteOff src i
        !b <- peekByteOff src (i + 1)
        !c <- peekByteOff src (i + 2)

        let (!w, !x, !y, !z) = encodeTriplet alpha a b c

        pokeByteOff dst j w
        pokeByteOff dst (j + 1) x

        if i + 1 < slen
        then pokeByteOff dst (j + 2) y
        else pokeByteOff dst (j + 2) _eq

        if i + 2 < slen
        then pokeByteOff dst (j + 3) z
        else pokeByteOff dst (j + 3) _eq

        go (i + 3) (j + 4)


-- | Naive implemementation. We can do better by copying directly to a 'Word32'
--
-- See: http://0x80.pl/notesen/2015-12-27-base64-encoding.html
--
encodeTriplet :: Addr# -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8)
encodeTriplet !alpha !a !b !c =
    let
      !w = shiftR a 2
      !x = (shiftL a 4 .&. 0x30) .|. (shiftR b 4)
      !y = (shiftL b 2 .&. 0x3c) .|. (shiftR c 6)
      !z = c .&. 0x3f
    in (ix w, ix x, ix y, ix z)
  where
    ix (W8# i) = W8# (indexWord8OffAddr# alpha (word2Int# i))
{-# INLINE encodeTriplet #-}
