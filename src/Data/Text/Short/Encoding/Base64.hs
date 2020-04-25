{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module       : Data.Text.Encoding.Base64
-- Copyright 	: (c) 2019-2020 Emily Pillmore
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
module Data.Text.Short.Encoding.Base64
( encodeBase64
, decodeBase64
, decodeBase64With
, decodeBase64Lenient
, isBase64
, isValidBase64
) where


import Data.Bifunctor (first)
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short.Base64 as BS64
import Data.Text (Text)
import qualified Data.Text.Encoding.Base64 as B64T
import Data.Text.Encoding.Base64.Error
import Data.Text.Short
import Data.Text.Short.Unsafe

-- | Encode a 'ShortText' value in Base64 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
encodeBase64 :: ShortText -> ShortText
encodeBase64 = fromByteStringUnsafe
  . B64.encodeBase64'
  . toByteString
{-# INLINE encodeBase64 #-}

-- | Decode a padded Base64-encoded 'ShortText' value
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64 :: ShortText -> Either Text ShortText
decodeBase64 = fmap fromText . B64T.decodeBase64 . toText
{-# INLINE decodeBase64 #-}

-- | Attempt to decode a 'ShortText' value as Base64, converting from
-- 'ByteString' to 'ShortText' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- Example:
--
-- @
-- 'decodeBase16With' 'T.decodeUtf8''
--   :: 'ShortText' -> 'Either' ('Base64Error' 'UnicodeException') 'ShortText'
-- @
--
decodeBase64With
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortText
      -- ^ Input text to decode
    -> Either (Base64Error err) ShortText
decodeBase64With f t = case BS64.decodeBase64 (toShortByteString t) of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64With #-}

-- | Leniently decode a Base64-encoded 'ShortText' value. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
decodeBase64Lenient :: ShortText -> ShortText
decodeBase64Lenient = fromText . B64T.decodeBase64Lenient . toText
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'ShortText' value is Base64-encoded.
--
isBase64 :: ShortText -> Bool
isBase64 = B64.isBase64 . toByteString
{-# INLINE isBase64 #-}

-- | Tell whether a 'ShortText' value is a valid Base64 format.
--
-- This will not tell you whether or not this is a correct Base64 representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'ShortText' value, use 'isBase64'.
--
isValidBase64 :: ShortText -> Bool
isValidBase64 = B64.isValidBase64 . toByteString
{-# INLINE isValidBase64 #-}
