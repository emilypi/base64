{-# language DataKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
-- |
-- Module       : Data.ByteString.Base64.Types.Internal
-- Copyright    : (c) 2019-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                sofia-m-a <https://github.com/sofia-m-a>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains the 'Base64' newtype, 'Alphabet'
-- datatype, and associated instances for 'Base64'.
--
module Data.Base64.Types.Internal
( Alphabet(..)
, Base64(..)
) where


import Control.DeepSeq (NFData, rnf)

-- | The different kinds of supported Base64 encodings
data Alphabet
  = StdPadded
    -- ^ Standard base64 according to [RFC 4648 §4](https://datatracker.ietf.org/doc/html/rfc4648#section-4)
    -- Padding is always inserted when encoding, and required when decoding
  | UrlPadded
    -- ^ Standard base64 according to [RFC 4648 §4](https://datatracker.ietf.org/doc/html/rfc4648#section-4)
    -- Padding is never inserted when encoding, and optional when decoding per
    -- [RFC 7049](https://datatracker.ietf.org/doc/html/rfc7049#section-2.4.4.2).
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

instance forall k a. (Eq a) => Eq (Base64 k a) where
  Base64 a == Base64 b = a == b

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
