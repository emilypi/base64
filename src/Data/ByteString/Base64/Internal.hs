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
( encodeBase64_
, decodeBase64_
) where


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

-- ---------------------------------------------------------------------------- --
-- C Foreign Imports

foreign import ccall unsafe "generic_base64_encode" c_encodeBase64
    :: Ptr Word8 -> Ptr Word8 -> Int -> Int

foreign import ccall unsafe "generic_base64_decode" c_decodeBase64
    :: Ptr Word8 -> Ptr Word8 -> Int -> Int

-- ---------------------------------------------------------------------------- --
-- Internal calls

encodeBase64_ :: ByteString -> ByteString
encodeBase64_ bs@(PS !sfp !soff !slen)
    | slen == 0 = mempty
    | slen > 1000000 = encodeBase64C dlen bs
    | otherwise = do
      unsafeCreate dlen $ \dp ->
        withForeignPtr sfp $ \sp ->
        encodeBase64H dp (plusPtr sp soff) slen
  where
    dlen = 4 * ((slen + 2) `div` 3)

encodeBase64H :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
encodeBase64H dptr src slen = go dptr 0
  where
    ix :: Addr# -> Word8 -> Word8
    ix addr (W8# i) = W8# (indexWord8OffAddr# addr (word2Int# i))
    {-# INLINE ix #-}

    go !dst !i
      | i >= slen - 2 = do
        let !n = slen - i
        if
          | n == 0 -> return ()
          | n == 1 -> do
            !t <- peekByteOff src i

            let !a = ix e0 t
                !b = ix e1 (shiftL (t .&. 0x03) 4)

            poke @Word8 dst a
            poke @Word8 (plusPtr dst 1) b
            poke @Word8 (plusPtr dst 2) 0x3d
            poke @Word8 (plusPtr dst 3) 0x3d

          | otherwise -> do
            !t <- peekByteOff src i
            !u <- peekByteOff src (i + 1)

            let !v = (shiftL (t .&. 0x03) 4) .|. ((shiftR u 4) .&. 0x0f)
                !w = shiftL (u .&. 0x0f) 2

            let !a = ix e0 t
                !b = ix e1 v
                !c = ix e2 w

            poke @Word8 dst a
            poke @Word8 (plusPtr dst 1) b
            poke @Word8 (plusPtr dst 2) c
            poke @Word8 (plusPtr dst 3) 0x3d
      | otherwise = do
        !x <- peekByteOff src i
        !y <- peekByteOff src (i + 1)
        !z <- peekByteOff src (i + 2)

        let !t = (shiftL (x .&. 0x03) 4) .|. ((shiftR y 4) .&. 0x0f)
            !u = (shiftL (y .&. 0x0f) 2) .|. ((shiftR z 6) .&. 0x03)

        let !a = ix e0 x
            !b = ix e1 t
            !c = ix e1 u
            !d = ix e2 z

        poke @Word8 dst a
        poke @Word8 (plusPtr dst 1) b
        poke @Word8 (plusPtr dst 2) c
        poke @Word8 (plusPtr dst 3) d

        go (plusPtr dst 4) (i + 3)

    !e0 = "AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHHIIIIJJJJKKKKLLLLMMMMNNNNOOOOPPPPQQQQRRRRSSSSTTTTUUUUVVVVWWWWXXXXYYYYZZZZaaaabbbbccccddddeeeeffffgggghhhhiiiijjjjkkkkllllmmmmnnnnooooppppqqqqrrrrssssttttuuuuvvvvwwwwxxxxyyyyzzzz0000111122223333444455556666777788889999++++////"#

    !e1 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#

    !e2 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#
{-# INLINE encodeBase64H #-}

encodeBase64C :: Int -> ByteString -> ByteString
encodeBase64C !dlen (PS !sfp !soff !slen) =
    unsafeCreate dlen $ \dp ->
      withForeignPtr sfp $ \sp ->
       let !l = c_encodeBase64 dp (plusPtr sp soff) slen
       in if l == -1 then error
         $ "Encoding failed at offset: "
         ++ show (plusPtr sp $ soff + l)
       else return ()
{-# INLINE encodeBase64C #-}

decodeBase64_ :: ByteString -> Either Text ByteString
decodeBase64_ (PS !sfp !soff !slen)
    | r /= 0 = Left "Invalid padding"
    | otherwise = unsafeDupablePerformIO $ do
      dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dp ->
        withForeignPtr sfp $ \sp ->
          let !l = c_decodeBase64 dp (plusPtr sp soff) slen
          in if l == -1 then return . Left . T.pack
            $ "Decoding failed at offset: "
            ++ show (plusPtr sp $ soff + l)
          else return $! Right (PS dfp 0 l)
  where
    (!q, !r) = divMod slen 4
    !dlen = q * 3
{-# INLINE decodeBase64_ #-}
