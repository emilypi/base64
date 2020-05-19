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
module Data.Text.Encoding.Base64
( encodeBase64
, decodeBase64
, decodeBase64With
, decodeBase64Lenient
, isBase64
, isValidBase64
) where


import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Base64.Error

-- | Encode a 'Text' value in Base64 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
encodeBase64 :: Text -> Text
encodeBase64 = B64.encodeBase64 . T.encodeUtf8
{-# INLINE encodeBase64 #-}

-- | Decode a padded Base64-encoded 'Text' value.
--
-- /Note:/ This function makes sure that decoding is total by deferring to
-- 'T.decodeLatin1'. This will always round trip for any valid Base64-encoded
-- text value, but it may not round trip for bad inputs. The onus is on the
-- caller to make sure inputs are valid. If unsure, defer to `decodeBase64With`
-- and pass in a custom decode function.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64 :: Text -> Either Text Text
decodeBase64 = fmap T.decodeLatin1 . B64.decodeBase64 . T.encodeUtf8
{-# INLINE decodeBase64 #-}

-- | Attempt to decode a 'Text' value as Base64, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- Example:
--
-- @
-- 'decodeBase64With' 'T.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base64Error' 'UnicodeException') 'Text'
-- @
--
decodeBase64With
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) Text
decodeBase64With f t = case B64.decodeBase64 t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64With #-}

-- | Leniently decode a Base64-encoded 'Text' value. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
decodeBase64Lenient :: Text -> Text
decodeBase64Lenient = T.decodeLatin1
    . B64.decodeBase64Lenient
    . T.encodeUtf8
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'Text' value is Base64-encoded.
--
isBase64 :: Text -> Bool
isBase64 = B64.isBase64 . T.encodeUtf8
{-# INLINE isBase64 #-}

-- | Tell whether a 'Text' value is a valid Base64 format.
--
-- This will not tell you whether or not this is a correct Base64 representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'Text' value, use 'isBase64'.
--
isValidBase64 :: Text -> Bool
isValidBase64 = B64.isValidBase64 . T.encodeUtf8
{-# INLINE isValidBase64 #-}
