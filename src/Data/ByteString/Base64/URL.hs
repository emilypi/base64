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
) where

import Data.ByteString (ByteString)
import Data.ByteString.Base64.Internal
import Data.Text (Text)


-- | Encode a 'ByteString' in base64-url with padding.
--
-- See: RFC-4648 section 5
--
encodeBase64 :: ByteString -> ByteString
encodeBase64 = encodeB64Padded base64UrlTable

-- | Decode a padded base64-url encoded 'ByteString'
--
-- See: RFC-4648 section 4
--
decodeBase64 :: ByteString -> Either Text ByteString
decodeBase64 = decodeB64 decodeB64UrlTable

-- | Encode a 'ByteString' in base64-url without padding.
--
-- Note: in some circumstances, the use of padding ("=") in base-encoded data
-- is not required or used. If you are absolutely sure the length of your
-- input data is divisible by 3, this function will be the same as 'encodeBase64'
-- with padding. However, if not, you may see garbage appended to output in the
-- form of "\NUL".
--
-- Only call unpadded variants when you can make assumptions about the length of
-- your input data.
--
-- See: RFC-4648 section 3.2
--
encodeBase64Unpadded :: ByteString -> ByteString
encodeBase64Unpadded = encodeB64Unpadded base64UrlTable

-- | Decode an unpadded base64-url encoded 'ByteString'
--
-- See: RFC-4648 section 4
--
decodeBase64Unpadded :: ByteString -> Either Text ByteString
decodeBase64Unpadded = decodeB64 decodeB64UrlTable
