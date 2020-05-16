{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PackageImports #-}
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
module Internal where


import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import "base64" Data.ByteString.Base64 as B64
import "base64" Data.ByteString.Base64.URL as B64U
import "base64" Data.ByteString.Lazy.Base64 as LB64
import "base64" Data.ByteString.Lazy.Base64.URL as LB64U
import "base64" Data.ByteString.Short.Base64 as SB64
import "base64" Data.ByteString.Short.Base64.URL as SB64U
import "base64" Data.Text.Encoding.Base64 as T64
import "base64" Data.Text.Encoding.Base64.URL as T64U
import "base64" Data.Text.Lazy.Encoding.Base64 as TL64
import "base64" Data.Text.Lazy.Encoding.Base64.URL as TL64U
import "base64" Data.Text.Short.Encoding.Base64 as TS64
import "base64" Data.Text.Short.Encoding.Base64.URL as TS64U


import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Short as TS

import Test.QuickCheck.Instances
import Test.QuickCheck hiding (label)


-- ------------------------------------------------------------------ --
-- Quickcheck instances

instance Arbitrary TS.ShortText where
  arbitrary = TS.fromText <$> arbitrary
  shrink xs = fmap TS.fromText $ shrink (TS.toText xs)

instance CoArbitrary TS.ShortText where
  coarbitrary = coarbitrary . TS.toText

instance Function TS.ShortText where
  function = functionMap
    (T.unpack . TS.toText)
    (TS.fromText . T.pack)

-- ------------------------------------------------------------------ --
-- Test Harnesses

data B64 = B64
data LB64 = LB64
data SB64 = SB64
data T64 = T64
data TL64 = TL64
data TS64 = TS64

-- | This class provides the generic API definition for
-- the base64 std alphabet

class
  ( Eq bs, Show bs
  , Arbitrary bs
  , CoArbitrary bs
  ) => Harness a bs | a -> bs
  where

  label :: String
  encode :: bs -> bs
  encodeUrl :: bs -> bs
  encodeUrlNopad :: bs -> bs

  decode :: bs -> Either Text bs
  decodeUrl :: bs -> Either Text bs
  decodeUrlPad :: bs -> Either Text bs
  decodeUrlNopad :: bs -> Either Text bs

  lenientUrl :: bs -> bs
  lenient :: bs -> bs

  validate :: bs -> Bool
  validateUrl :: bs -> Bool


instance Harness B64 BS.ByteString where
  label = "ByteString"

  encode = B64.encodeBase64'
  decode = B64.decodeBase64
  lenient = B64.decodeBase64Lenient
  validate = B64.isBase64
  encodeUrl = B64U.encodeBase64'
  encodeUrlNopad = B64U.encodeBase64Unpadded'
  decodeUrl = B64U.decodeBase64
  decodeUrlPad = B64U.decodeBase64Padded
  decodeUrlNopad = B64U.decodeBase64Unpadded
  lenientUrl = B64U.decodeBase64Lenient
  validateUrl = B64U.isBase64Url

instance Harness LB64 LBS.ByteString where
  label = "Lazy ByteString"

  encode = LB64.encodeBase64'
  decode = LB64.decodeBase64
  lenient = LB64.decodeBase64Lenient
  validate = LB64.isBase64
  encodeUrl = LB64U.encodeBase64'
  encodeUrlNopad = LB64U.encodeBase64Unpadded'
  decodeUrl = LB64U.decodeBase64
  decodeUrlPad = LB64U.decodeBase64Padded
  decodeUrlNopad = LB64U.decodeBase64Unpadded
  lenientUrl = LB64U.decodeBase64Lenient
  validateUrl = LB64U.isBase64Url

instance Harness SB64 SBS.ShortByteString where
  label = "Short ByteString"

  encode = SB64.encodeBase64'
  decode = SB64.decodeBase64
  lenient = SB64.decodeBase64Lenient
  validate = SB64.isBase64
  encodeUrl = SB64U.encodeBase64'
  encodeUrlNopad = SB64U.encodeBase64Unpadded'
  decodeUrl = SB64U.decodeBase64
  decodeUrlPad = SB64U.decodeBase64Padded
  decodeUrlNopad = SB64U.decodeBase64Unpadded
  lenientUrl = SB64U.decodeBase64Lenient
  validateUrl = SB64U.isBase64Url

instance Harness T64 Text where
  label = "Text"

  encode = T64.encodeBase64
  decode = T64.decodeBase64
  lenient = T64.decodeBase64Lenient
  validate = T64.isBase64
  encodeUrl = T64U.encodeBase64
  encodeUrlNopad = T64U.encodeBase64Unpadded
  decodeUrl = T64U.decodeBase64
  decodeUrlPad = T64U.decodeBase64Padded
  decodeUrlNopad = T64U.decodeBase64Unpadded
  lenientUrl = T64U.decodeBase64Lenient
  validateUrl = T64U.isBase64Url

instance Harness TL64 TL.Text where
  label = "Lazy Text"

  encode = TL64.encodeBase64
  decode = TL64.decodeBase64
  lenient = TL64.decodeBase64Lenient
  validate = TL64.isBase64
  encodeUrl = TL64U.encodeBase64
  encodeUrlNopad = TL64U.encodeBase64Unpadded
  decodeUrl = TL64U.decodeBase64
  decodeUrlPad = TL64U.decodeBase64Padded
  decodeUrlNopad = TL64U.decodeBase64Unpadded
  lenientUrl = TL64U.decodeBase64Lenient
  validateUrl = TL64U.isBase64Url

instance Harness TS64 TS.ShortText where
  label = "Short Text"

  encode = TS64.encodeBase64
  decode = TS64.decodeBase64
  lenient = TS64.decodeBase64Lenient
  validate = TS64.isBase64
  encodeUrl = TS64U.encodeBase64
  encodeUrlNopad = TS64U.encodeBase64Unpadded
  decodeUrl = TS64U.decodeBase64
  decodeUrlPad = TS64U.decodeBase64Padded
  decodeUrlNopad = TS64U.decodeBase64Unpadded
  lenientUrl = TS64U.decodeBase64Lenient
  validateUrl = TS64U.isBase64Url
