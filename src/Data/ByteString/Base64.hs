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
, decodeBase64
, encodeBase64Unpadded
, decodeBase64Unpadded
, decodeBase64Lenient
, isBase64
) where


import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base64.Internal
import Data.Text (Text)


-- | Encode a 'ByteString' in base64 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
encodeBase64 :: ByteString -> ByteString
encodeBase64 = encodeBase64_ True base64Table
{-# INLINE encodeBase64 #-}

-- | Decode a padded base64-encoded 'ByteString'
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64 :: ByteString -> Either Text ByteString
decodeBase64 = decodeBase64_ False decodeB64Table
{-# INLINE decodeBase64 #-}

-- | Encode a 'ByteString' in base64 without padding.
--
-- __Note:__ in some circumstances, the use of padding ("=") in base-encoded data
-- is not required or used. This is not one of them. If you are absolutely sure
-- the length of your bytestring is divisible by 3, this function will be the same
-- as 'encodeBase64' with padding, however, if not, you may see garbage appended to
-- your bytestring.
--
-- Only call unpadded variants when you can make assumptions about the length of
-- your input data.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase64Unpadded :: ByteString -> ByteString
encodeBase64Unpadded = encodeBase64_ False base64Table
{-# INLINE encodeBase64Unpadded #-}

-- | Decode an unpadded base64-encoded 'ByteString'.
--
-- __Note:__ Only call unpadded variants when you can make assumptions
-- about the length of your input data.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
decodeBase64Unpadded :: ByteString -> Either Text ByteString
decodeBase64Unpadded = decodeBase64_ False decodeB64Table
{-# INLINE decodeBase64Unpadded #-}

-- | Leniently decode an unpadded base64-encoded 'ByteString'. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = decodeBase64Lenient_ decodeB64Table
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'Bytestring' value is base64-encoded
--
isBase64 :: ByteString -> Bool
isBase64 = BS.all (`BS.elem` alphabet)
  where
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
