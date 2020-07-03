{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Base64
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.ByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base64
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.ByteString.Base64
( -- * Encoding
  encodeBase64
, encodeBase64'
  -- * Decoding
, decodeBase64
, decodeBase64Lenient
  -- * Validation
, isBase64
, isValidBase64
) where


import Data.ByteString.Internal (ByteString(..))
import Data.ByteString.Base64.Internal
import Data.ByteString.Base64.Internal.Head
import Data.ByteString.Base64.Internal.Tables
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text.Encoding as T

import System.IO.Unsafe


-- | Encode a 'ByteString' value as Base64 'Text' with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> encodeBase64 "Sun"
-- "U3Vu"
--
encodeBase64 :: ByteString -> Text
encodeBase64 = T.decodeUtf8 . encodeBase64'
{-# inline encodeBase64 #-}

-- | Encode a 'ByteString' value as a Base64 'ByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> encodeBase64' "Sun"
-- "U3Vu"
--
encodeBase64' :: ByteString -> ByteString
encodeBase64' = encodeBase64_ base64Table
{-# inline encodeBase64' #-}

-- | Decode a padded Base64-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64 "U3Vu"
-- Right "Sun"
--
-- >>> decodeBase64 "U3V"
-- Left "Base64-encoded bytestring requires padding"
--
-- >>> decodebase64 "U3V="
-- Left "non-canonical encoding detected at offset: 2"
--
decodeBase64 :: ByteString -> Either Text ByteString
decodeBase64 bs@(PS _ _ !l)
    | l == 0 = Right bs
    | r == 1 = Left "Base64-encoded bytestring has invalid size"
    | r /= 0 = Left "Base64-encoded bytestring requires padding"
    | otherwise = unsafeDupablePerformIO $ decodeBase64_ dlen decodeB64Table bs
  where
    !q = l `quot` 4
    !r = l `rem` 4
    !dlen = q * 3
{-# inline decodeBase64 #-}

-- | Leniently decode an unpadded Base64-encoded 'ByteString' value. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
-- === __Examples__:
--
-- >>> decodeBase64Lenient "U3Vu"
-- "Sun"
--
-- >>> decodeBase64Lenient "U3V"
-- "Su"
--
-- >>> decodebase64Lenient "U3V="
-- "Su"
--
decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = decodeBase64Lenient_ decodeB64Table
{-# inline decodeBase64Lenient #-}

-- | Tell whether a 'ByteString' value is base64 encoded.
--
-- This function will also detect non-canonical encodings such as @ZE==@, which are
-- externally valid Base64url-encoded values, but are internally inconsistent "impossible"
-- values.
--
-- === __Examples__:
--
-- >>> isBase64 "U3Vu"
-- True
--
-- >>> isBase64 "U3V"
-- False
--
-- >>> isBase64 "U3V="
-- False
--
isBase64 :: ByteString -> Bool
isBase64 bs = isValidBase64 bs && isRight (decodeBase64 bs)
{-# inline isBase64 #-}

-- | Tell whether a 'ByteString' value is a valid Base64 format.
--
-- This will not tell you whether or not this is a correct Base64url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'ByteString' value, use 'isBase64'.
--
-- === __Examples__:
--
-- >>> isValidBase64 "U3Vu"
-- True
--
-- >>> isValidBase64 "U3V"
-- True
--
-- >>> isValidBase64 "U3V="
-- True
--
-- >>> isValidBase64 "%"
-- False
--
isValidBase64 :: ByteString -> Bool
isValidBase64 = validateBase64 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
{-# inline isValidBase64 #-}
