{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.Text.Encoding.Base64
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
module Data.Text.Encoding.Base64
( encodeBase64
, decodeBase64
, encodeBase64Unpadded
, decodeBase64Unpadded
, decodeBase64Lenient
, isBase64
) where


import qualified Data.ByteString.Base64 as B64

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Encode a 'Text' value in Base64 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
encodeBase64 :: Text -> Text
encodeBase64 = B64.encodeBase64 . T.encodeUtf8
{-# INLINE encodeBase64 #-}

-- | Decode a padded Base64-encoded 'Text' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64 :: Text -> Either Text Text
decodeBase64 = fmap T.decodeUtf8 . B64.decodeBase64 . T.encodeUtf8
{-# INLINE decodeBase64 #-}

-- | Encode a 'Text' value in Base64 without padding.
--
-- __Note:__ in some circumstances, the use of padding ("=") in base-encoded data
-- is not required or used. This is not one of them. If you are absolutely sure
-- the length of your text is divisible by 3, this function will be the same
-- as 'encodeBase64' with padding, however, if not, you may see garbage appended to
-- your text.
--
-- Only call unpadded variants when you can make assumptions about the length of
-- your input data.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase64Unpadded :: Text -> Text
encodeBase64Unpadded = B64.encodeBase64Unpadded . T.encodeUtf8
{-# INLINE encodeBase64Unpadded #-}

-- | Decode an unpadded Base64-encoded 'Text'
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
decodeBase64Unpadded :: Text -> Either Text Text
decodeBase64Unpadded = fmap T.decodeUtf8
    . B64.decodeBase64Unpadded
    . T.encodeUtf8
{-# INLINE decodeBase64Unpadded #-}

-- | Leniently decode a Base64-encoded 'Text' value. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
decodeBase64Lenient :: Text -> Text
decodeBase64Lenient = T.decodeUtf8
    . B64.decodeBase64Lenient
    . T.encodeUtf8
{-# INLINE decodeBase64Lenient #-}


-- | Tell whether a 'Text' value is Base64-encoded
--
isBase64 :: Text -> Bool
isBase64 = T.all (isJust . flip T.find alphabet . (==))
  where
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
{-# INLINE isBase64 #-}
