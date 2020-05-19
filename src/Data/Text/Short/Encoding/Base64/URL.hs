-- |
-- Module       : Data.Text.Short.Encoding.Base64.URL
-- Copyright 	: (c) 2019-2020 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base64-URL encoding including
-- unpadded and lenient variants
module Data.Text.Short.Encoding.Base64.URL
( encodeBase64
, decodeBase64
, decodeBase64With
, encodeBase64Unpadded
, decodeBase64Unpadded
, decodeBase64UnpaddedWith
, decodeBase64Padded
, decodeBase64PaddedWith
, decodeBase64Lenient
, isBase64Url
, isValidBase64Url
) where


import Data.Bifunctor (first)
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short.Base64.URL as BS64U
import Data.Text (Text)
import qualified Data.Text.Encoding.Base64.URL as B64TU
import Data.Text.Encoding.Base64.Error
import Data.Text.Short
import Data.Text.Short.Unsafe

-- | Encode a 'ShortText' value in Base64url with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-5 RFC-4648 section 5>
--
encodeBase64 :: ShortText -> ShortText
encodeBase64 = fromByteStringUnsafe
  . B64U.encodeBase64'
  . toByteString
{-# INLINE encodeBase64 #-}

-- | Decode a padded Base64url-encoded 'ShortText' value. If its length is not a multiple
-- of 4, then padding chars will be added to fill out the input to a multiple of
-- 4 for safe decoding as base64url encodings are optionally padded.
--
-- For a decoder that fails on unpadded input, use 'decodeBase64Unpadded'.
--
-- /Note:/ This function makes sure that decoding is total by deferring to
-- 'T.decodeLatin1'. This will always round trip for any valid Base64-encoded
-- text value, but it may not round trip for bad inputs. The onus is on the
-- caller to make sure inputs are valid. If unsure, defer to `decodeBase64With`
-- and pass in a custom decode function.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64 :: ShortText -> Either Text ShortText
decodeBase64 = fmap fromText . B64TU.decodeBase64 . toText
{-# INLINE decodeBase64 #-}

-- | Attempt to decode a 'ShortByteString' value as Base64url, converting from
-- 'ByteString' to 'ShortText' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 4>
--
-- Example:
--
-- @
-- 'decodeBase64With' 'T.decodeUtf8''
--   :: 'ShortByteString' -> 'Either' ('Base64Error' 'UnicodeException') 'ShortText'
-- @
--
decodeBase64With
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) ShortText
decodeBase64With f t = case BS64U.decodeBase64 t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64With #-}

-- | Encode a 'ShortText' value in Base64url without padding. Note that for Base64url,
-- padding is optional. If you call this function, you will simply be encoding
-- as Base64url and stripping padding chars from the output.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-3.2 RFC-4648 section 3.2>
--
encodeBase64Unpadded :: ShortText -> ShortText
encodeBase64Unpadded = fromByteStringUnsafe
  . B64U.encodeBase64Unpadded'
  . toByteString
{-# INLINE encodeBase64Unpadded #-}

-- | Decode an unpadded Base64url encoded 'ShortText' value.
--
-- /Note:/ This function makes sure that decoding is total by deferring to
-- 'T.decodeLatin1'. This will always round trip for any valid Base64-encoded
-- text value, but it may not round trip for bad inputs. The onus is on the
-- caller to make sure inputs are valid. If unsure, defer to `decodeBase64UnpaddedWith`
-- and pass in a custom decode function.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64Unpadded :: ShortText -> Either Text ShortText
decodeBase64Unpadded = fmap fromText . B64TU.decodeBase64Unpadded . toText
{-# INLINE decodeBase64Unpadded #-}

-- | Attempt to decode an unpadded 'ShortByteString' value as Base64url, converting from
-- 'ShortByteString' to 'ShortText' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 4>
--
-- Example:
--
-- @
-- 'decodeBase64With' 'T.decodeUtf8''
--   :: 'ShortByteString' -> 'Either' ('Base64Error' 'UnicodeException') 'ShortText'
-- @
--
decodeBase64UnpaddedWith
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) ShortText
decodeBase64UnpaddedWith f t = case BS64U.decodeBase64Unpadded t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64UnpaddedWith #-}

-- | Decode an padded Base64url encoded 'ShortText' value
--
-- /Note:/ This function makes sure that decoding is total by deferring to
-- 'T.decodeLatin1'. This will always round trip for any valid Base64-encoded
-- text value, but it may not round trip for bad inputs. The onus is on the
-- caller to make sure inputs are valid. If unsure, defer to `decodeBase64PaddedWith`
-- and pass in a custom decode function.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
decodeBase64Padded :: ShortText -> Either Text ShortText
decodeBase64Padded = fmap fromText . B64TU.decodeBase64Padded . toText
{-# INLINE decodeBase64Padded #-}

-- | Attempt to decode a padded 'ShortByteString' value as Base64url, converting from
-- 'ByteString' to 'ShortText' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 4>
--
-- Example:
--
-- @
-- 'decodeBase64With' 'T.decodeUtf8''
--   :: 'ShortByteString' -> 'Either' ('Base64Error' 'UnicodeException') 'ShortText'
-- @
--
decodeBase64PaddedWith
    :: (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ShortByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) ShortText
decodeBase64PaddedWith f t = case BS64U.decodeBase64Padded t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64PaddedWith #-}

-- | Leniently decode an unpadded Base64url-encoded 'ShortText'. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
decodeBase64Lenient :: ShortText -> ShortText
decodeBase64Lenient = fromText . B64TU.decodeBase64Lenient . toText
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'ShortText' value is Base64url-encoded.
--
isBase64Url :: ShortText -> Bool
isBase64Url = B64U.isBase64Url . toByteString
{-# INLINE isBase64Url #-}

-- | Tell whether a 'ShortText' value is a valid Base64url format.
--
-- This will not tell you whether or not this is a correct Base64url representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'ShortText' value, use 'isBase64Url'.
--
isValidBase64Url :: ShortText -> Bool
isValidBase64Url = B64U.isValidBase64Url . toByteString
{-# INLINE isValidBase64Url #-}
