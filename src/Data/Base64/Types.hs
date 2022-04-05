{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language Trustworthy #-}
{-# language UndecidableInstances #-}
-- |
-- Module       : Data.ByteString.Base64.Types
-- Copyright    : (c) 2019-2022 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>,
--                sofia-m-a <https://github.com/sofia-m-a>
-- Stability    : stable
-- Portability  : non-portable
--
-- This module contains the 'Base64' type definition, 'Alphabet'
-- datatype, alphabet constraints, and various quality of life
-- combinators for working with 'Base64'-wrapped data.
--
module Data.Base64.Types
( Alphabet(..)
, type Base64
, assertBase64
, extractBase64
, coerceBase64
, type UrlAlphabet
, type StdAlphabet
, type NonStandardAlphabet
) where


import Data.Base64.Types.Internal (Alphabet(..), Base64(..))
import Data.Coerce (coerce)
import Data.Kind

import GHC.TypeLits


-- | Assert a value to be encoded in a specific way
--
-- /Warning/: This is a blind assertion that a particular
-- value is base64 encoded in some alphabet. If you are not
-- sure of the provenance of the value, you may experience
-- odd behavior when attempting to decode. Use at your own
-- risk. If I see any issues logged on this project from
-- negligent use of this or 'coerceBase64', Sofia and I will
-- smite you.
--
assertBase64 :: forall k a. a -> Base64 k a
assertBase64 = Base64

-- | Forget that a particular value is Base64-encoded
--
extractBase64 :: forall a k. Base64 k a -> a
extractBase64 (Base64 a) = a

-- | Coerce the alphabet of a base64-encoded bytestring
--
-- /Warning/: This is a blind assertion that a particular
-- value is base64 encoded in some alphabet. If you are not
-- sure of the provenance of the value, you may experience
-- odd behavior when attempting to decode. Use at your own
-- risk. If I see any issues logged on this project from
-- negligent use of this or 'assertBase64', Sofia and I will
-- smite you.
--
coerceBase64 :: forall j k a. Base64 k a -> Base64 j a
coerceBase64 = coerce

-- | The type family of Url-safe alphabets
--
-- This type family defines the union of compatible Url-safe base64 types
--
type family UrlAlphabet k :: Constraint where
  UrlAlphabet 'UrlPadded = ()
  UrlAlphabet 'UrlUnpadded = ()
  UrlAlphabet _ = TypeError
    ( 'Text "Cannot prove base64 value is encoded using the url-safe \
            \alphabet. Please re-encode using the url-safe encoders, or use \
            \a lenient decoder for the url-safe alphabet instead."
    )

-- | The type family of Std alphabets
--
-- This type family defines the union of compatible standard
-- alphabet base64 types
--
type family StdAlphabet k :: Constraint where
  StdAlphabet 'StdPadded = ()
  StdAlphabet _ = TypeError
    ( 'Text "Cannot prove base64 value is encoded using the std \
            \alphabet. Please re-encode using the std encoders, or use \
            \a lenient decoder for the std alphabet instead."
    )

type family NonStandardAlphabet k :: Constraint where
  NonStandardAlphabet 'NonStandard = ()
  NonStandardAlphabet _ = TypeError
    ( 'Text "Cannot prove base64 value is encoded using a non-standard \
            \alphabet. Please re-encode and assert/coerce the alphabet, \
            \and use a lenient decoder."
    )
