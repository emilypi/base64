{-# LANGUAGE OverloadedStrings #-}
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
, decodeBase64Lenient
, isBase64Url
) where


import qualified Data.ByteString.Base64.URL as B64U

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Encode a 'Text' in base64-url with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase64 :: Text -> Text
encodeBase64 = B64U.encodeBase64 . T.encodeUtf8
{-# INLINE encodeBase64 #-}

-- | Decode a padded base64-url encoded 'ByteString'. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as base64url encodings are optionally padded.
--
-- For a decoder that fails on unpadded input of incorrect size, use 'decodeBase64Unpadded'.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64 :: Text -> Either Text Text
decodeBase64 = fmap T.decodeUtf8 . B64U.decodeBase64 . T.encodeUtf8
{-# INLINE decodeBase64 #-}

-- | Encode a 'ByteString' in base64-url without padding. Note that for Base64url,
-- padding is optional. If you call this function, you will simply be encoding
-- as base64 and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase64Unpadded :: Text -> Text
encodeBase64Unpadded = B64U.encodeBase64Unpadded . T.encodeUtf8
{-# INLINE encodeBase64Unpadded #-}

-- | Decode an unpadded base64-url encoded 'Text' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64Unpadded :: Text -> Either Text Text
decodeBase64Unpadded = fmap T.decodeUtf8
    . B64U.decodeBase64Unpadded
    . T.encodeUtf8
{-# INLINE decodeBase64Unpadded #-}

-- | Leniently decode an unpadded base64url-encoded 'Text'. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
decodeBase64Lenient :: Text -> Text
decodeBase64Lenient = T.decodeUtf8
    . B64U.decodeBase64Lenient
    . T.encodeUtf8
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'Text' value is base64-encoded
--
isBase64Url :: Text -> Bool
isBase64Url = T.all (isJust . flip T.find alphabet . (==))
  where
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
{-# INLINE isBase64Url #-}
