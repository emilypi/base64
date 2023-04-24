{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.ByteString.Short.Base64
-- Copyright    : (c) 2019-2022 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.ByteString.Short.ShortByteString'-valued combinators for
-- implementing the RFC 4648 specification of the Base64
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.ByteString.Short.Base64
( -- * Encoding
  encodeBase64
, encodeBase64'
  -- * Decoding
, decodeBase64
, decodeBase64Untyped
, decodeBase64Lenient
  -- * Validation
, isBase64
, isValidBase64
) where

import Data.Base64.Types
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Short.Unsafe (fromShortByteStringUnsafe)

-- $setup
--
-- >>> import Data.Base64.Types
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
--


-- | Encode a 'ShortByteString' value as Base64 'ShortText' with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> encodeBase64 "Sun"
-- "U3Vu"
--
encodeBase64 :: ShortByteString -> Base64 'StdPadded ShortText
encodeBase64 = fmap fromShortByteStringUnsafe . encodeBase64'
{-# INLINE encodeBase64 #-}

-- | Encode a 'ShortByteString' value as a Base64 'ShortByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> encodeBase64' "Sun"
-- "U3Vu"
--
encodeBase64' :: ShortByteString -> Base64 'StdPadded ShortByteString
encodeBase64' = fmap toShort . B64.encodeBase64' . fromShort
{-# INLINE encodeBase64' #-}

-- | Decode a padded Base64-encoded 'ShortByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64 $ assertBase64 @'StdPadded "U3Vu"
-- "Sun"
--
decodeBase64
  :: StdAlphabet k
  => Base64 k ShortByteString
  -> ShortByteString
decodeBase64 = toShort . B64.decodeBase64 . fmap fromShort
{-# INLINE decodeBase64 #-}

-- | Decode a padded Base64-encoded 'ShortByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Untyped "U3Vu"
-- Right "Sun"
--
-- >>> decodeBase64Untyped "U3V"
-- Left "Base64-encoded bytestring requires padding"
--
-- >>> decodeBase64Untyped "U3V="
-- Left "non-canonical encoding detected at offset: 2"
--
decodeBase64Untyped :: ShortByteString -> Either Text ShortByteString
decodeBase64Untyped = fmap toShort . B64.decodeBase64Untyped . fromShort
{-# inline decodeBase64Untyped #-}

-- | Leniently decode an unpadded Base64-encoded 'ShortByteString' value. This function
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
-- >>> decodeBase64Lenient "U3V="
-- "Su"
--
decodeBase64Lenient :: ShortByteString -> ShortByteString
decodeBase64Lenient = toShort . B64.decodeBase64Lenient . fromShort
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'ShortByteString' value is base64 encoded.
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
isBase64 :: ShortByteString -> Bool
isBase64 = B64.isBase64 . fromShort
{-# INLINE isBase64 #-}

-- | Tell whether a 'ShortByteString' value is a valid Base64 format.
--
-- This will not tell you whether or not this is a correct Base64url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'ShortByteString' value, use 'isBase64'.
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
isValidBase64 :: ShortByteString -> Bool
isValidBase64 = B64.isValidBase64 . fromShort
{-# INLINE isValidBase64 #-}
