{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base64.Internal
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Internal module defining the encoding and decoding
-- processes and tables.
--
module Data.ByteString.Base64.Internal
( validateBase64
, validateLastPad
) where


import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Data.Text (Text)

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import GHC.Word

import System.IO.Unsafe

-- | Given a bytestring, check to see that it conforms to a given alphabet
--
validateBase64 :: ByteString -> ByteString -> Bool
validateBase64 !alphabet (PS !fp !off !l) =
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

validateLastPad
    :: ByteString
    -> IO (Either Text ByteString)
    -> Either Text ByteString
validateLastPad (PS !fp !o !l) io = unsafeDupablePerformIO $
    withForeignPtr fp $ \p -> do
      let !end = l + o
      a <- peek @Word8 (plusPtr p (end - 1))
      if a == 0x3d
      then return $ Left "Base64-encoded bytestring has invalid padding"
      else io
{-# INLINE validateLastPad #-}
