{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.ByteString.Base64
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base64 encoding including
-- unpadded and lenient variants
--
module Data.ByteString.Base64
( encodeBase64
, encodeBase64'
, decodeBase64
, decodeBase64Lenient
, isBase64
, isValidBase64
) where


import Data.ByteString (ByteString)
import Data.ByteString.Base64.Internal
import Data.ByteString.Base64.Internal.Head
import Data.ByteString.Base64.Internal.Tables
import Data.ByteString.Base64.Internal.Types
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text.Encoding as T


-- | Encode a 'ByteString' value as Base64 'Text' with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
encodeBase64 :: ByteString -> Text
encodeBase64 = T.decodeUtf8 . encodeBase64'
{-# INLINE encodeBase64 #-}

-- | Encode a 'ByteString' value as a Base64 'ByteString'  value with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
encodeBase64' :: ByteString -> ByteString
encodeBase64' = encodeBase64_
{-# INLINE encodeBase64' #-}

-- | Decode a padded Base64-encoded 'ByteString' value.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- /Note:/ This function is not RFC compliant, and __will__ add padding to an
-- unpadded Base64-encoded value for decoding. For strictly RFC-compliant decoding,
-- use 'decodeBase64Unpadded'.
--
decodeBase64 :: ByteString -> Either Text ByteString
decodeBase64 = decodeBase64_ NoPad decodeB64Table
{-# INLINE decodeBase64 #-}

-- | Leniently decode an unpadded Base64-encoded 'ByteString' value. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = decodeBase64Lenient_ decodeB64Table
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'ByteString' value is base64 encoded.
--
isBase64 :: ByteString -> Bool
isBase64 bs = isValidBase64 bs && isRight (decodeBase64 bs)
{-# INLINE isBase64 #-}

-- | Tell whether a 'ByteString' value is a valid Base64 format.
--
-- This will not tell you whether or not this is a correct Base64url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'ByteString' value, use 'isBase64'.
--
isValidBase64 :: ByteString -> Bool
isValidBase64 = validateBase64 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
{-# INLINE isValidBase64 #-}
