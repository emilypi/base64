{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Base64.Internal
-- Copyright    : (c) 2019-2022 Emily Pillmore
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
, validateBase64Url
, validateLastPad
) where


import Data.Base64.Types.Internal
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Data.Text (Text)

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

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

validateBase64Url :: ByteString -> ByteString -> Bool
validateBase64Url !alphabet bs@(PS _ _ l)
    | l == 0 = True
    | r == 0 = f bs
    | r == 2 = f (BS.append bs "==")
    | r == 3 = f (BS.append bs "=")
    | otherwise = False

  where
    r = l `rem` 4

    f (PS fp o n) = accursedUnutterablePerformIO $
      withForeignPtr fp $ \p -> go (plusPtr p o) (plusPtr p (n + o))

    go !p !end
      | p == end = return True
      | otherwise = do
        w <- peek p

        let check a
              | a == 0x3d, plusPtr p 1 == end = True
              | a == 0x3d, plusPtr p 2 == end = True
              | a == 0x3d = False
              | otherwise = BS.elem a alphabet

        if check w then go (plusPtr p 1) end else return False
{-# INLINE validateBase64Url #-}

-- | This function checks that the last char of a bytestring is '='
-- and, if true, fails with a message or completes some io action.
--
-- This is necessary to check when decoding permissively (i.e. filling in padding chars).
-- Consider the following 4 cases of a string of length l:
--
-- l = 0 mod 4: No pad chars are added, since the input is assumed to be good.
-- l = 1 mod 4: Never an admissible length in base64
-- l = 2 mod 4: 2 padding chars are added. If padding chars are present in the string, they will fail as to decode as final quanta
-- l = 3 mod 4: 1 padding char is added. In this case  a string is of the form <body> + <padchar>. If adding the
-- pad char "completes"" the string so that it is `l = 0 mod 4`, then this may possibly be forming corrupting data.
-- This case is degenerate and should be disallowed.
--
-- Hence, permissive decodes should only fill in padding chars when it makes sense to add them. That is,
-- if an input is degenerate, it should never succeed when we add padding chars. We need the following invariant to hold:
--
-- @
--   B64U.decodeUnpadded <|> B64U.decodePadded ~ B64U.decodePadded
-- @
--
validateLastPad
    :: Base64 k ByteString
    -> IO (Either Text ByteString)
    -> Either Text ByteString
validateLastPad (Base64 !bs) io
    | BS.last bs == 0x3d = Left "Base64-encoded bytestring has invalid padding"
    | otherwise = unsafeDupablePerformIO io
{-# INLINE validateLastPad #-}
