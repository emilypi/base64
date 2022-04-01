{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Base64.Internal(Alphabet(..), Base64(..), mapBase64) where

-- | The different kinds of supported Base64 encodings
data Alphabet
    -- | Standard base64 according to [RFC 4648 §4](https://datatracker.ietf.org/doc/html/rfc4648#section-4)
    -- Padding is always inserted when encoding, and required when decoding
    = StdPadded
    -- | Standard base64 according to [RFC 4648 §4](https://datatracker.ietf.org/doc/html/rfc4648#section-4)
    -- Padding is never inserted when encoding, and optional when decoding
    | StdUnpadded
    -- | URL-safe base64 according to [RFC 4648 §5](https://datatracker.ietf.org/doc/html/rfc4648#section-5) aka base64url
    -- Padding is always inserted when encoding, and required when decoding
    | UrlPadded
    -- | URL-safe base64 according to [RFC 4648 §5](https://datatracker.ietf.org/doc/html/rfc4648#section-5) aka base64url
    -- Padding is never inserted when encoding, and optional when decoding
    | UrlUnpadded

-- | Wraps a value, asserting that it is or is intended to be in a particular kind of Base64 encoding
-- use 'extractBase64' to extract the value, and 'assertBase64' to tag a value as base64-encoded
newtype Base64 (k :: Alphabet) a = Base64 a

mapBase64 :: (a -> b) -> Base64 k a -> Base64 k b
mapBase64 f (Base64 a) = Base64 (f a)