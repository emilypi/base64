{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
-- |
-- Module       : Data.Text.Encoding.Base64
-- Copyright    : (c) 2019-2022 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains 'Data.Text.Text'-valued combinators for
-- implementing the RFC 4648 specification of the Base64
-- encoding format. This includes lenient decoding variants, as well as
-- internal and external validation for canonicity.
--
module Data.Text.Encoding.Base64
( -- * Encoding
  encodeBase64
  -- * Decoding
, decodeBase64
, decodeBase64Untyped
, decodeBase64UntypedWith
, decodeBase64Lenient
  -- * Validation
, isBase64
, isValidBase64
) where


import Data.Base64.Types
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64

import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Base64.Error

-- $setup
--
-- >>> import Data.Base64.Types
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
-- >>> :set -XDataKinds
--

-- | Encode a 'Text' value in Base64 with padding.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> encodeBase64 "Sun"
-- "U3Vu"
--
encodeBase64 :: Text -> Base64 'StdPadded Text
encodeBase64 = B64.encodeBase64 . T.encodeUtf8
{-# INLINE encodeBase64 #-}

-- | Decode a padded Base64-encoded 'Text' value.
--
-- /Note:/ This function makes sure that decoding is total by deferring to
-- 'T.decodeUtf8'. This will always round trip for any valid Base64-encoded
-- text value, but it may not round trip for bad inputs. The onus is on the
-- caller to make sure inputs are valid. If unsure, defer to `decodeBase64With`
-- and pass in a custom decode function.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64 $ assertBase64 @'StdPadded "U3Vu"
-- "Sun"
--
decodeBase64 :: StdAlphabet k => Base64 k Text -> Text
decodeBase64 = T.decodeUtf8 . B64.decodeBase64 . fmap T.encodeUtf8
{-# INLINE decodeBase64 #-}

-- | Decode a padded Base64-encoded 'Text' value.
--
-- /Note:/ This function makes sure that decoding is total by deferring to
-- 'T.decodeUtf8'. This will always round trip for any valid Base64-encoded
-- text value, but it may not round trip for bad inputs. The onus is on the
-- caller to make sure inputs are valid. If unsure, defer to `decodeBase64With`
-- and pass in a custom decode function.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Examples__:
--
-- >>> decodeBase64Untyped "U3Vu"
-- Right "Sun"
--
decodeBase64Untyped :: Text -> Either Text Text
decodeBase64Untyped = fmap T.decodeUtf8
  . B64.decodeBase64Untyped
  . T.encodeUtf8
{-# INLINE decodeBase64Untyped #-}

-- | Attempt to decode a 'Text' value as Base64, converting from
-- 'ByteString' to 'Text' according to some encoding function. In practice,
-- This is something like 'decodeUtf8'', which may produce an error.
--
-- See: <https://tools.ietf.org/html/rfc4648#section-4 RFC-4648 section 4>
--
-- === __Example__:
--
-- @
-- 'decodeBase64UntypedWith' 'T.decodeUtf8''
--   :: 'ByteString' -> 'Either' ('Base64Error' 'UnicodeException') 'Text'
-- @
--
decodeBase64UntypedWith
    :: (ByteString -> Either err Text)
      -- ^ convert a bytestring to text (e.g. 'T.decodeUtf8'')
    -> ByteString
      -- ^ Input text to decode
    -> Either (Base64Error err) Text
decodeBase64UntypedWith f t = case B64.decodeBase64Untyped t of
  Left de -> Left $ DecodeError de
  Right a -> first ConversionError (f a)
{-# INLINE decodeBase64UntypedWith #-}

-- | Leniently decode a Base64-encoded 'Text' value. This function
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
-- >>> decodeBase64Lenient "U3V="
-- "Su"
--
decodeBase64Lenient :: Text -> Text
decodeBase64Lenient = T.decodeUtf8
    . B64.decodeBase64Lenient
    . T.encodeUtf8
{-# INLINE decodeBase64Lenient #-}

-- | Tell whether a 'Text' value is Base64-encoded.
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
isBase64 :: Text -> Bool
isBase64 = B64.isBase64 . T.encodeUtf8
{-# INLINE isBase64 #-}

-- | Tell whether a 'Text' value is a valid Base64 format.
--
-- This will not tell you whether or not this is a correct Base64 representation,
-- only that it conforms to the correct shape. To check whether it is a true
-- Base64 encoded 'Text' value, use 'isBase64'.
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
isValidBase64 :: Text -> Bool
isValidBase64 = B64.isValidBase64 . T.encodeUtf8
{-# INLINE isValidBase64 #-}
