{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language Trustworthy #-}
module Data.Base64
( Alphabet(..)
, Base64
, assertBase64
, extractBase64
, coerceBase64
, UrlAlphabet
, StdAlphabet
) where


import Data.Base64.Internal (Alphabet(..), Base64(..))
import Data.Coerce (coerce)

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
type family UrlAlphabet k :: Bool where
  UrlAlphabet 'UrlPadded = 'True
  UrlAlphabet 'UrlUnpadded = 'True
  UrlAlphabet _ = 'False

-- | The type family of Std alphabets
--
-- This type family defines the union of compatible standard
-- alphabet base64 types
--
type family StdAlphabet k :: Bool where
  StdAlphabet 'StdPadded = 'True
  StdAlphabet _ = 'False
