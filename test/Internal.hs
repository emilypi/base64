{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module       : Main
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains internal test harnesses for `base64`
--
module Internal
( Harness(..)
, b64
, lb64
, sb64
, t64
, tl64
, ts64
, TextHarness(..)
, tt64
, ttl64
, tts64
) where


import "base64" Data.Base64.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import "base64" Data.ByteString.Base64 as B64
import "base64" Data.ByteString.Base64.URL as B64U
import "base64" Data.ByteString.Lazy.Base64 as LB64
import "base64" Data.ByteString.Lazy.Base64.URL as LB64U
import "base64" Data.ByteString.Short.Base64 as SB64
import "base64" Data.ByteString.Short.Base64.URL as SB64U
import Data.Text (Text)
import qualified Data.Text as T
import "base64" Data.Text.Encoding.Base64 as T64
import "base64" Data.Text.Encoding.Base64.URL as T64U
import Data.Text.Encoding.Base64.Error (Base64Error(..))
import qualified Data.Text.Lazy as TL
import "base64" Data.Text.Lazy.Encoding.Base64 as TL64
import "base64" Data.Text.Lazy.Encoding.Base64.URL as TL64U
import qualified Data.Text.Short as TS
import "base64" Data.Text.Short.Encoding.Base64 as TS64
import "base64" Data.Text.Short.Encoding.Base64.URL as TS64U

import Test.QuickCheck hiding (label)

-- ------------------------------------------------------------------ --
-- Test Harnesses

-- | This dictionary provides the generic API definition for
-- the base64 std alphabet
--
data Harness bs = Harness
  { label :: String
  , encode :: bs -> Base64 'StdPadded bs
  , encodeUrl :: bs -> Base64 'UrlPadded bs
  , encodeUrlNopad :: bs -> Base64 'UrlUnpadded bs
  , decode :: bs -> Either Text bs
  , decodeTyped :: Base64 'StdPadded bs -> bs
  , decodeUrl :: bs -> Either Text bs
  , decodeUrlPad :: bs -> Either Text bs
  , decodeUrlNopad :: bs -> Either Text bs
  , decodeUrlTyped :: forall k. UrlAlphabet k => Base64 k bs -> bs
  , decodeUrlTypedPad :: Base64 'UrlPadded bs -> bs
  , decodeUrlTypedNopad :: Base64 'UrlUnpadded bs -> bs
  , lenientUrl :: bs -> bs
  , lenient :: bs -> bs
  , correct :: bs -> Bool
  , correctUrl :: bs -> Bool
  , validate :: bs -> Bool
  , validateUrl :: bs -> Bool
  }


b64 :: Harness BS.ByteString
b64 = Harness
  { label = "ByteString"
  , encode = B64.encodeBase64'
  , decode = B64.decodeBase64Untyped
  , decodeTyped = B64.decodeBase64
  , lenient = B64.decodeBase64Lenient
  , correct = B64.isBase64
  , validate = B64.isValidBase64
  , encodeUrl = B64U.encodeBase64'
  , encodeUrlNopad = B64U.encodeBase64Unpadded'
  , decodeUrl = B64U.decodeBase64Untyped
  , decodeUrlPad = B64U.decodeBase64PaddedUntyped
  , decodeUrlNopad = B64U.decodeBase64UnpaddedUntyped
  , decodeUrlTyped = B64U.decodeBase64
  , decodeUrlTypedPad = B64U.decodeBase64Padded
  , decodeUrlTypedNopad = B64U.decodeBase64Unpadded
  , lenientUrl = B64U.decodeBase64Lenient
  , correctUrl = B64U.isBase64Url
  , validateUrl = B64U.isValidBase64Url
  }

lb64 :: Harness LBS.ByteString
lb64 = Harness
  { label = "Lazy ByteString"
  , encode = LB64.encodeBase64'
  , decode = LB64.decodeBase64Untyped
  , decodeTyped = LB64.decodeBase64
  , lenient = LB64.decodeBase64Lenient
  , correct = LB64.isBase64
  , validate = LB64.isValidBase64
  , encodeUrl = LB64U.encodeBase64'
  , encodeUrlNopad = LB64U.encodeBase64Unpadded'
  , decodeUrl = LB64U.decodeBase64Untyped
  , decodeUrlPad = LB64U.decodeBase64PaddedUntyped
  , decodeUrlNopad = LB64U.decodeBase64UnpaddedUntyped
  , decodeUrlTyped = LB64U.decodeBase64
  , decodeUrlTypedPad = LB64U.decodeBase64Padded
  , decodeUrlTypedNopad = LB64U.decodeBase64Unpadded
  , lenientUrl = LB64U.decodeBase64Lenient
  , correctUrl = LB64U.isBase64Url
  , validateUrl = LB64U.isValidBase64Url
  }

sb64 :: Harness SBS.ShortByteString
sb64 = Harness
  { label = "Short ByteString"
  , encode = SB64.encodeBase64'
  , decode = SB64.decodeBase64Untyped
  , decodeTyped = SB64.decodeBase64
  , lenient = SB64.decodeBase64Lenient
  , correct = SB64.isBase64
  , validate = SB64.isValidBase64
  , encodeUrl = SB64U.encodeBase64'
  , encodeUrlNopad = SB64U.encodeBase64Unpadded'
  , decodeUrl = SB64U.decodeBase64Untyped
  , decodeUrlPad = SB64U.decodeBase64PaddedUntyped
  , decodeUrlNopad = SB64U.decodeBase64UnpaddedUntyped
  , decodeUrlTyped = SB64U.decodeBase64
  , decodeUrlTypedPad = SB64U.decodeBase64Padded
  , decodeUrlTypedNopad = SB64U.decodeBase64Unpadded
  , lenientUrl = SB64U.decodeBase64Lenient
  , correctUrl = SB64U.isBase64Url
  , validateUrl = SB64U.isValidBase64Url
  }

t64 :: Harness Text
t64 = Harness
  { label = "Text"
  , encode = T64.encodeBase64
  , decode = T64.decodeBase64Untyped
  , decodeTyped = T64.decodeBase64
  , lenient = T64.decodeBase64Lenient
  , correct = T64.isBase64
  , validate = T64.isValidBase64
  , encodeUrl = T64U.encodeBase64
  , encodeUrlNopad = T64U.encodeBase64Unpadded
  , decodeUrl = T64U.decodeBase64Untyped
  , decodeUrlPad = T64U.decodeBase64PaddedUntyped
  , decodeUrlNopad = T64U.decodeBase64UnpaddedUntyped
  , decodeUrlTyped = T64U.decodeBase64
  , decodeUrlTypedPad = T64U.decodeBase64Padded
  , decodeUrlTypedNopad = T64U.decodeBase64Unpadded
  , lenientUrl = T64U.decodeBase64Lenient
  , correctUrl = T64U.isBase64Url
  , validateUrl = T64U.isValidBase64Url
  }

tl64 :: Harness TL.Text
tl64 = Harness
  { label = "Lazy Text"
  , encode = TL64.encodeBase64
  , decode = TL64.decodeBase64Untyped
  , decodeTyped = TL64.decodeBase64
  , lenient = TL64.decodeBase64Lenient
  , correct = TL64.isBase64
  , validate = TL64.isValidBase64
  , encodeUrl = TL64U.encodeBase64
  , encodeUrlNopad = TL64U.encodeBase64Unpadded
  , decodeUrl = TL64U.decodeBase64Untyped
  , decodeUrlPad = TL64U.decodeBase64PaddedUntyped
  , decodeUrlNopad = TL64U.decodeBase64UnpaddedUntyped
  , decodeUrlTyped = TL64U.decodeBase64
  , decodeUrlTypedPad = TL64U.decodeBase64Padded
  , decodeUrlTypedNopad = TL64U.decodeBase64Unpadded
  , lenientUrl = TL64U.decodeBase64Lenient
  , correctUrl = TL64U.isBase64Url
  , validateUrl = TL64U.isValidBase64Url
  }

ts64 :: Harness TS.ShortText
ts64 = Harness
  { label = "Short Text"
  , encode = TS64.encodeBase64
  , decode = TS64.decodeBase64Untyped
  , decodeTyped = TS64.decodeBase64
  , lenient = TS64.decodeBase64Lenient
  , correct = TS64.isBase64
  , validate = TS64.isValidBase64
  , encodeUrl = TS64U.encodeBase64
  , encodeUrlNopad = TS64U.encodeBase64Unpadded
  , decodeUrl = TS64U.decodeBase64Untyped
  , decodeUrlPad = TS64U.decodeBase64PaddedUntyped
  , decodeUrlNopad = TS64U.decodeBase64UnpaddedUntyped
  , decodeUrlTyped = TS64U.decodeBase64
  , decodeUrlTypedPad = TS64U.decodeBase64Padded
  , decodeUrlTypedNopad = TS64U.decodeBase64Unpadded
  , lenientUrl = TS64U.decodeBase64Lenient
  , correctUrl = TS64U.isBase64Url
  , validateUrl = TS64U.isValidBase64Url
  }

-- -------------------------------------------------------------------- --
-- Text-specific harness

data TextHarness bs cs = TextHarness
  { decodeWith_ :: forall err. (bs -> Either err cs) -> bs -> Either (Base64Error err) cs
  , decodeUrlWith_ :: forall err. (bs -> Either err cs) -> bs -> Either (Base64Error err) cs
  , decodeUrlPaddedWith_ :: forall err. (bs -> Either err cs) -> bs -> Either (Base64Error err) cs
  , decodeUrlUnpaddedWith_ :: forall err. (bs -> Either err cs) -> bs -> Either (Base64Error err) cs
  }

tt64 :: TextHarness BS.ByteString Text
tt64 = TextHarness
  { decodeWith_ = T64.decodeBase64UntypedWith
  , decodeUrlWith_ = T64U.decodeBase64UntypedWith
  , decodeUrlPaddedWith_ = T64U.decodeBase64PaddedUntypedWith
  , decodeUrlUnpaddedWith_ = T64U.decodeBase64UnpaddedUntypedWith
  }

ttl64 :: TextHarness LBS.ByteString TL.Text
ttl64 = TextHarness
  { decodeWith_ = TL64.decodeBase64UntypedWith
  , decodeUrlWith_ = TL64U.decodeBase64UntypedWith
  , decodeUrlPaddedWith_ =  TL64U.decodeBase64PaddedUntypedWith
  , decodeUrlUnpaddedWith_ =  TL64U.decodeBase64UnpaddedUntypedWith
  }

tts64 :: TextHarness SBS.ShortByteString TS.ShortText
tts64 = TextHarness
  { decodeWith_ = TS64.decodeBase64UntypedWith
  , decodeUrlWith_ = TS64U.decodeBase64UntypedWith
  , decodeUrlPaddedWith_ = TS64U.decodeBase64PaddedUntypedWith
  , decodeUrlUnpaddedWith_ = TS64U.decodeBase64UnpaddedUntypedWith
  }

-- ------------------------------------------------------------------ --
-- Quickcheck instances

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary
    shrink xs = BS.pack <$> shrink (BS.unpack xs)

instance CoArbitrary BS.ByteString where
    coarbitrary = coarbitrary . BS.unpack

instance Arbitrary LBS.ByteString where
    arbitrary = LBS.pack <$> arbitrary
    shrink xs = LBS.pack <$> shrink (LBS.unpack xs)

instance CoArbitrary LBS.ByteString where
    coarbitrary = coarbitrary . LBS.unpack

instance Arbitrary SBS.ShortByteString where
    arbitrary = SBS.pack <$> arbitrary
    shrink xs = SBS.pack <$> shrink (SBS.unpack xs)

instance CoArbitrary SBS.ShortByteString where
    coarbitrary = coarbitrary . SBS.unpack

instance Arbitrary T.Text where
    arbitrary = T.pack . getUnicodeString <$> arbitrary
    shrink xs = T.pack . getUnicodeString <$> shrink (UnicodeString $ T.unpack xs)

instance Arbitrary TL.Text where
    arbitrary = TL.pack . getUnicodeString <$> arbitrary
    shrink xs = TL.pack . getUnicodeString <$> shrink (UnicodeString $ TL.unpack xs)

instance CoArbitrary T.Text where
    coarbitrary = coarbitrary . T.unpack

instance CoArbitrary TL.Text where
    coarbitrary = coarbitrary . TL.unpack

instance Arbitrary TS.ShortText where
  arbitrary = TS.fromText <$> arbitrary
  shrink xs = TS.fromText <$> shrink (TS.toText xs)

instance CoArbitrary TS.ShortText where
  coarbitrary = coarbitrary . TS.toText
