-- |
-- Module       : Data.ByteString.Base64.URL
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base64-URL encoding including
-- unpadded and lenient variants
--
module Data.ByteString.Base64.URL
( encodeBase64
, decodeBase64
, encodeBase64Unpadded
, decodeBase64Unpadded
, decodeBase64Lenient
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base64.Internal
import Data.Either (fromRight)
import Data.Text (Text)


-- | Encode a 'ByteString' in base64-url with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase64 :: ByteString -> ByteString
encodeBase64 = encodeBase64_ True base64UrlTable

-- | Decode a padded base64-url encoded 'ByteString'. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as base64url encodings are optionally padded.
--
-- For a decoder that fails on unpadded input of incorrect size, use 'decodeBase64Unpadded'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64 :: ByteString -> Either Text ByteString
decodeBase64 = decodeBase64_ True decodeB64UrlTable

-- | Encode a 'ByteString' in base64-url without padding. Note that for Base64url,
-- padding is optional. If you call this function, you will simply be encoding
-- as base64 and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase64Unpadded :: ByteString -> ByteString
encodeBase64Unpadded = BS.takeWhile ((/=) 0x3d) . encodeBase64_ True base64UrlTable

-- | Decode a padded base64-url encoded 'ByteString'. If its length is not a multiple
-- of 4, then padding chars will /not/ be added to fill out the input to a multiple of
-- 4.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64Unpadded :: ByteString -> Either Text ByteString
decodeBase64Unpadded = decodeBase64_ False decodeB64UrlTable

-- | Decode an unpadded base64-url encoded 'ByteString'. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the bytestring.
--
-- __Note:__ this function is not RFC 4648 compliant, but covers a common use-case in
-- base6url encoded data in which padding is considered optional. In general you should
-- prefer to call this function if you're ever working with foreign encoded data.
--
decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = undefined
{-# INLINE decodeBase64Lenient #-}
