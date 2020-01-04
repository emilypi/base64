{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
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
encodeBase64_ (PS !sfp !soff !slen) =
    unsafeCreate dlen $ \dp ->
      withForeignPtr sfp $ \sp ->
       let !l = c_encodeBase64 dp (plusPtr sp soff) slen
       in if l == -1 then error
         $ "Encoding failed at offset: "
         <> show (plusPtr sp $ soff + l)
       else return ()
  where
    !dlen = 4 * ((slen + 2) `div` 3)

decodeBase64_ :: ByteString -> Either Text ByteString
decodeBase64_ (PS !sfp !soff !slen)
    | r /= 0 = Left "invalid padding"
    | otherwise = unsafeDupablePerformIO $ do
      dfp <- mallocPlainForeignPtrBytes dlen
      withForeignPtr dfp $ \dp ->
        withForeignPtr sfp $ \sp ->
          let !l = c_decodeBase64 dp (plusPtr sp soff) slen
              !_ = unsafeDupablePerformIO $ print l
          in if l == -1 then return . Left . T.pack
             $ "Decoding from Base64 failed - invalid padding at offset: "
             <> show (plusPtr sp $ soff + l)
          else return $! Right (PS dfp 0 l)
  where
    (!q, !r) = divMod slen 4
    !dlen = q * 3
