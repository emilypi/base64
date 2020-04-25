-- |
-- Module       : Data.ByteString.Short.Base64
-- Copyright    : (c) 2019 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base64 encoding including
-- unpadded and lenient variants
--
module Data.ByteString.Short.Base64
( encodeBase64
, encodeBase64'
, decodeBase64
, decodeBase64Lenient
, isBase64
, isValidBase64
) where


import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Either (isRight)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Text.Short.Unsafe (fromShortByteStringUnsafe)

-- | Encode a 'ShortByteString' value as Base64 'Text' with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
encodeBase64 :: ShortByteString -> ShortText
encodeBase64 = fromShortByteStringUnsafe . encodeBase64'
{-# INLINE encodeBase64 #-}

-- | Encode a 'ShortByteString' value as a Base64 'ShortByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
encodeBase64' :: ShortByteString -> ShortByteString
encodeBase64' = toShort . B64.encodeBase64' . fromShort
{-# INLINE encodeBase64' #-}

-- | Decode a padded Base64-encoded 'ShortByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- /Note:/ This function is not RFC compliant, and __will__ add padding to an
-- unpadded Base64-encoded value for decoding. For strictly RFC-compliant decoding,
-- use 'decodeBase64Unpadded'.
--
decodeBase64 :: ShortByteString -> Either Text ShortByteString
decodeBase64 = fmap toShort . B64.decodeBase64 . fromShort
{-# INLINE decodeBase64 #-}

-- | Leniently decode an unpadded Base64-encoded 'ShortByteString' value. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
decodeBase64Lenient :: ShortByteString -> ShortByteString
decodeBase64Lenient = toShort . B64.decodeBase64Lenient . fromShort
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'ShortByteString' value is base64 encoded.
--
isBase64 :: ShortByteString -> Bool
isBase64 bs = isValidBase64 bs && isRight (decodeBase64 bs)
{-# INLINE isBase64 #-}

-- | Tell whether a 'ShortByteString' value is a valid Base64 format.
--
-- This will not tell you whether or not this is a correct Base64url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'ShortByteString' value, use 'isBase64'.
--
isValidBase64 :: ShortByteString -> Bool
isValidBase64 = B64.isValidBase64 . fromShort
{-# INLINE isValidBase64 #-}
