{-# language RankNTypes #-}
{-# language DataKinds #-}
{-# language TypeFamilies #-}
{-# language Trustworthy #-}
module Data.Base64.Types
( Alphabet(..)
, Base64
, assertBase64
, extractBase64
, coerceBase64
, UrlAlphabet
, StdAlphabet
) where


import Control.DeepSeq (NFData, rnf)
import Data.Coerce (coerce)

-- | The different kinds of supported Base64 encodings
data Alphabet
  = StdPadded
    -- ^ Standard base64 according to [RFC 4648 §4](https://datatracker.ietf.org/doc/html/rfc4648#section-4)
    -- Padding is always inserted when encoding, and required when decoding
  | UrlPadded
    -- ^ Standard base64 according to [RFC 4648 §4](https://datatracker.ietf.org/doc/html/rfc4648#section-4)
    -- Padding is never inserted when encoding, and optional when decoding
  | UrlUnpadded
    -- ^ URL-safe base64 according to [RFC 4648 §5](https://datatracker.ietf.org/doc/html/rfc4648#section-5) aka base64url
    -- Padding is never inserted when encoding, and optional when decoding
  | NonStandard
    -- ^ Any non-standard, non RFC 4648-compliant base64 encoding.
    -- Can only be decoded using lenient decoders.

-- | Wraps a value, asserting that it is or is intended to be
-- in a particular kind of Base64 encoding use 'extractBase64'
-- to extract the value, and 'assertBase64' to tag a value
-- as base64-encoded
--
newtype Base64 (k :: Alphabet) a = Base64 a

instance forall k. Functor (Base64 k) where
  fmap f (Base64 a) = Base64 (f a)

instance forall k. Applicative (Base64 k) where
  pure = Base64
  Base64 f <*> Base64 a = Base64 (f a)

instance forall k. Monad (Base64 k) where
  return = pure
  Base64 a >>= k = k a

instance forall k a. (Show a) => Show (Base64 k a) where
  show (Base64 a) = show a

instance forall k a. NFData a => NFData (Base64 k a) where
  rnf (Base64 a) = rnf a

-- | Assert a value to be encoded in a specific way
--
assertBase64 :: forall k a. a -> Base64 k a
assertBase64 = Base64

-- | Forget that a particular value is Base64-encoded
--
extractBase64 :: Base64 k a -> a
extractBase64 (Base64 a) = a

-- | Coerce the alphabet of a base64-encoded bytestring
--
coerceBase64 :: Base64 k a -> Base64 j a
coerceBase64 = coerce

-- | The type family of Url-safe alphabets
--
-- This type family defines the union of compatible Url-safe base64 types
--
type family UrlAlphabet k :: Constraint where
  UrlAlphabet 'UrlPadded = ()
  UrlAlphabet 'UrlUnpadded = ()
  UrlAlphabet _ = TypeError
    ('Text
      "Not a url-safe alphabet-encoded base64 value. \
      \Consider using the url-safe lenientDecode.")


-- | The type family of Std alphabets
--
-- This type family defines the union of compatible standard
-- alphabet base64 types
--
type family StdAlphabet k :: Constraint where
  StdAlphabet 'StdPadded = ()
  StdAlphabet _ = TypeError
    ('Text "Not a std alphabet-encoded base64 value. \
           \Consider using lenientDecode.")
