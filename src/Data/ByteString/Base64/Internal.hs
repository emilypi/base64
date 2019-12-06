{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
module Data.ByteString.Base64.Internal
( base64
, base64'
, base64Lazy
) where


import Control.Monad (when)

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as LBS

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Exts
import GHC.ForeignPtr
import GHC.Storable
import GHC.Word

import System.IO.Unsafe


base64Lazy :: Bool -> LBS.ByteString -> LBS.ByteString
base64Lazy padding = LBS.fromChunks
   . fmap (base64 padding)
   . chunkN 3
   . LBS.toChunks

data EncodeTable = ET (Ptr Word8) (Ptr Word16)

mkEncodeTable :: Addr# -> EncodeTable
mkEncodeTable !alphabet = ET (Ptr alphabet) (Ptr eptr)
  where
    ix (I# i) = W8# (indexWord8OffAddr# alphabet i)
    (PS (ForeignPtr eptr _) _ _) = BS.pack
      $ concat
      $ [ [ix j, ix k] | j <- [0..63], k <- [0..63] ]

base64 :: Bool -> ByteString -> ByteString
base64 padded (PS sfp soff slen) =
    unsafeCreate dlen $ \dptr ->
    withForeignPtr sfp $ \sptr ->
      base64Internal' aptr eptr sptr (castPtr dptr) (sptr `plusPtr` (soff + slen)) padded
  where
    !dlen = ((slen + 2) `div` 3) * 4
    ET aptr eptr = mkEncodeTable alpha
    !alpha =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#

base64Internal'
    :: Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> Ptr Word16
    -> Ptr Word8
    -> Bool
    -> IO ()
base64Internal' alpha etable sptr dptr end padded = go sptr dptr
  where
    ix :: Int -> IO Word8
    ix n = peek (alpha `plusPtr` n)
    {-# INLINE ix #-}

    _eq = 0x3d :: Word8
    {-# INLINE _eq #-}

    w32 :: Word8 -> Word32
    w32 = fromIntegral
    {-# INLINE w32 #-}

    go !src !dst
      | src `plusPtr` 2 >= end = complete src (castPtr dst)
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

    complete :: Ptr Word8 -> Ptr Word8 -> IO ()
    complete !src !dst
      | src == end = return ()
      | otherwise = do
        let peekSP n f = (f . fromIntegral) `fmap` peek @Word8 (src `plusPtr` n)
            !twoMore = src `plusPtr` 2 == end

        !a <- peekSP 0 ((`shiftR` 2) . (.&. 0xfc))
        !b <- peekSP 0 ((`shiftL` 4) . (.&. 0x03))
        !b' <- if twoMore
          then peekSP 1 ((.|. b) . (`shiftR` 4) . (.&. 0xf0))
          else return b

        poke dst =<< ix a
        poke (plusPtr dst 1) =<< ix b'

        if twoMore then do
          idx <- peekSP 1 ((`shiftL` 2) . (.&. 0x0f))
          c <- ix idx
          poke (dst `plusPtr` 2) c
          when padded (poke (dst `plusPtr` 3) _eq)
        else do
          poke (dst `plusPtr` 2) _eq
          when padded (poke (dst `plusPtr` 3) _eq)
{-# INLINE base64Internal' #-}


-- | Chunk a list of bytestrings into chunks that are sized
-- a multiple of n.
--
chunkN :: Int -> [ByteString] -> [ByteString]
chunkN _ [] = []
chunkN !n (b:bs) = case BS.length b `divMod` n of
    (_, 0) -> b : chunkN n bs
    (d, _) ->
      let ~(p, s) = BS.splitAt (d * n) b
      in p : go s bs
  where
    go acc [] = [acc]
    go acc (z:zs) =
      let ~(p, s) = BS.splitAt (n - BS.length acc) z
      in
        let acc' = acc <> p
        in if BS.length acc' == n
        then
          let zs' = if BS.null s then zs else s:zs
          in acc' : chunkN n zs'
        else go acc' zs


-- -- | my std reference impl
-- --
-- base64Internal :: Addr# -> Ptr Word8 -> Ptr Word8 -> Int -> Bool -> IO ()
-- base64Internal alpha src dst slen padded = go 0 0
--   where
--     _eq = 0x3d :: Word8

--     go !i !j
--       | i  >= slen = return ()
--       | otherwise = do
--         a <- peekByteOff src i
--         b <- peekByteOff src (i + 1)
--         c <- peekByteOff src (i + 2)

--         let (!w, !x, !y, !z) = encodeTriplet alpha a b c

--         pokeByteOff dst j w
--         pokeByteOff dst (j + 1) x

--         if i + 1 < slen
--         then pokeByteOff dst (j + 2) y
--         else when padded (pokeByteOff dst (j + 2) _eq)

--         if i + 2 < slen
--         then pokeByteOff dst (j + 3) z
--         else when padded (pokeByteOff dst (j + 3) _eq)

--         go (i + 3) (j + 4)


-- -- | Naive implemementation. We can do better by copying directly to a 'Word32'
-- --
-- -- See: http://0x80.pl/notesen/2015-12-27-base64-encoding.html
-- --
-- encodeTriplet :: Addr# -> Word8 -> Word8 -> Word8 -> (Word8, Word8, Word8, Word8)
-- encodeTriplet alpha a b c =
--     let
--       !w = shiftR a 2
--       !x = (shiftL a 4 .&. 0x30) .|. (shiftR b 4)
--       !y = (shiftL b 2 .&. 0x3c) .|. (shiftR c 6)
--       !z = c .&. 0x3f
--     in (ix w, ix x, ix y, ix z)
--   where
--     ix (W8# i) = W8# (indexWord8OffAddr# alpha (word2Int# i))
-- {-# INLINE encodeTriplet #-}
