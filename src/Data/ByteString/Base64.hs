{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
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
, decodeBase64Lenient
, encodeBase64Unpadded
, decodeBase64Unpadded
, decodeBase64UnpaddedLenient
) where


import Data.ByteString (ByteString)
import Data.ByteString.Base64.Internal
import Data.Text (Text)


-- | Encode a 'ByteString' in base64 with padding.
--
-- See: RFC-4648 section 4
--
encodeBase64 :: ByteString -> ByteString
encodeBase64 = base64Padded

decodeBase64 :: ByteString -> Either Text ByteString
decodeBase64 = undefined

decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = undefined

-- | Encode a 'ByteString' in base64 without padding.
--
-- Note: in some circumstances, the use of padding ("=") in base-encoded data
-- is not required or used. This is not one of them. If you are absolutely sure
-- the length of your bytestring is divisible by 3, this function will be the same
-- as 'encodeBase64' with padding, however, if not, you may see garbage appended to
-- your bytestring in the form of "\NUL".
--
-- Only call unpadded variants when you can make assumptions about the length of
-- your input data.
--
-- See: RFC-4648 section 3.2
--
encodeBase64Unpadded :: ByteString -> ByteString
encodeBase64Unpadded = base64Unpadded

decodeBase64Unpadded :: ByteString -> Either Text ByteString
decodeBase64Unpadded = undefined

decodeBase64UnpaddedLenient :: ByteString -> ByteString
decodeBase64UnpaddedLenient = undefined
