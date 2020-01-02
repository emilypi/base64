-- |
-- Module       : Data.Text.Encoding.Base64.URL
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
module Data.Text.Encoding.Base64.URL
( encodeBase64
, decodeBase64
, encodeBase64Unpadded
, decodeBase64Unpadded
) where


import qualified Data.ByteString.Base64.URL as B64U

import Data.Text (Text)
import qualified Data.Text.Encoding as T

-- | Encode a 'Text' in base64-url with padding.
--
-- See: RFC-4648 section 5
--
encodeBase64 :: Text -> Text
encodeBase64 = T.decodeUtf8 . B64U.encodeBase64 . T.encodeUtf8

-- | Decode a padded base64-url encoded 'Text'
--
-- See: RFC-4648 section 4
--
decodeBase64 :: Text -> Either Text Text
decodeBase64 = fmap T.decodeUtf8 . B64U.decodeBase64 . T.encodeUtf8

-- | Encode a 'Text' value in base64-url without padding.
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
encodeBase64Unpadded :: Text -> Text
encodeBase64Unpadded = T.decodeUtf8
    . B64U.encodeBase64Unpadded
    . T.encodeUtf8

-- | Decode an unpadded base64-url encoded 'Text' value
--
-- See: RFC-4648 section 4
--
decodeBase64Unpadded :: Text -> Either Text Text
decodeBase64Unpadded = fmap T.decodeUtf8
    . B64U.decodeBase64Unpadded
    . T.encodeUtf8
