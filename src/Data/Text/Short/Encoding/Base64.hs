{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module       : Data.Text.Short.Encoding.Base64
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Short.ShortText'-valued combinators
-- implementing the RFC 4648 specification for the Base64
-- encoding format. This includes lenient decoding variants, and
-- external + internal validations for canonicity.
--
module Data.Text.Short.Encoding.Base64
( -- * Encoding
  encodeBase64
  -- * Decoding
, decodeBase64
, decodeBase64With
, decodeBase64Lenient
  -- * Validation
, isBase64
, isValidBase64
) where

import Data.Base64

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
-- === __Examples__:
--
-- >>> encodeBase64 "Sun"
-- "U3Vu"
--
encodeBase64 :: ShortText -> Base64 'StdPadded ShortText
encodeBase64 = fmap fromByteStringUnsafe
  . B64.encodeBase64'
  . toByteString
{-# INLINE encodeBase64 #-}

-- | Decode a padded Base64-encoded 'ShortText' value
--
-- /Note:/ This function makes sure that decoding is total by deferring to
-- 'T.decodeLatin1'. This will always round trip for any valid Base64-encoded
-- text value, but it may not round trip for bad inputs. The onus is on the
-- caller to make sure inputs are valid. If unsure, defer to `decodeBase64With`
-- and pass in a custom decode function.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64 "U3Vu"
-- Right "Sun"
--
-- >>> decodeBase64 "U3V"
-- Left "Base64-encoded bytestring requires padding"
--
-- >>> decodebase64 "U3V="
-- Left "non-canonical encoding detected at offset: 2"
--
decodeBase64 :: StdAlphabet k => Base64 k ShortText -> Either Text ShortText
decodeBase64 = fmap fromText . B64T.decodeBase64 . fmap toText
{-# INLINE decodeBase64 #-}

-- | Attempt to decode a 'ShortByteString' value as Base64, converting from
-- 'ByteString' to 'ShortText' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Example__:
--
-- @
-- 'decodeBase64With' 'T.decodeUtf8''
--   :: 'ShortByteString' -> 'Either' ('Base64Error' 'UnicodeException') 'ShortText'
-- @
--
decodeBase64With
    :: StdAlphabet k
    => (ShortByteString -> Either err ShortText)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> Base64 k ShortByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) ShortText
decodeBase64With f t = case BS64.decodeBase64 t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64With #-}

-- | Leniently decode a Base64-encoded 'ShortText' value. This function
-- will not generate parse errors. If input data contains padding chars,
-- then the input will be parsed up until the first pad character.
--
-- __Note:__ This is not RFC 4648-compliant.
--
-- === __Examples__:
--
-- >>> decodeBase64Lenient "U3Vu"
-- "Sun"
--
-- >>> decodeBase64Lenient "U3V"
-- "Su"
--
-- >>> decodebase64Lenient "U3V="
-- "Su"
--
decodeBase64Lenient :: Base64 k ShortText -> ShortText
decodeBase64Lenient = fromText . B64T.decodeBase64Lenient . fmap toText
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'ShortText' value is Base64-encoded.
--
-- === __Examples__:
--
-- >>> isBase64 "U3Vu"
-- True
--
-- >>> isBase64 "U3V"
-- False
--
-- >>> isBase64 "U3V="
-- False
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
-- === __Examples__:
--
-- >>> isValidBase64 "U3Vu"
-- True
--
-- >>> isValidBase64 "U3V"
-- True
--
-- >>> isValidBase64 "U3V="
-- True
--
-- >>> isValidBase64 "%"
-- False
--
isValidBase64 :: ShortText -> Bool
isValidBase64 = B64.isValidBase64 . toByteString
{-# INLINE isValidBase64 #-}
