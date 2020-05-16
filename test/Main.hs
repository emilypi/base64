{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Main
-- Copyright    : (c) 2019-2020 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains the test implementation for the `base64` package
--
module Main
( main
, tests
) where


import qualified Data.ByteString as BS
import "base64" Data.ByteString.Base64 as B64
import "base64" Data.ByteString.Base64.URL as B64U
import qualified "base64-bytestring" Data.ByteString.Base64 as Bos
import qualified "base64-bytestring" Data.ByteString.Base64.URL as BosU

import Internal

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Base64 Tests"
  [ mkTree B64
  , mkTree LB64
  , mkTree SB64
  , mkTree T64
  , mkTree TL64
  , mkTree TS64
  ]

mkTree :: forall a b. Harness a b => a -> TestTree
mkTree _ = testGroup (label @a)
  [ properties @a undefined
  , rfcVectors
  , paddingTests
  ]

-- ---------------------------------------------------------------- --
-- Property tests

properties :: forall a b. Harness a b => a -> TestTree
properties _ = testGroup "Property tests"
  [ prop_roundtrip @a undefined
  , prop_correctness @a undefined
  , prop_url_padding @a undefined
  , prop_bos_coherence
  ]

prop_roundtrip :: forall a b. Harness a b => a -> TestTree
prop_roundtrip _ = testGroup "prop_roundtrip"
  [ testProperty "prop_std_roundtrip" $ \bs ->
      Right bs == decode @a (encode @a bs)
  , testProperty "prop_url_roundtrip" $ \bs ->
      Right bs == decodeUrl @a (encodeUrl @a bs)
  , testProperty "prop_url_roundtrip_nopad" $ \bs ->
      Right bs == decodeUrl @a (encodeUrlNopad @a bs)
  ]

prop_correctness :: forall a b. Harness a b => a -> TestTree
prop_correctness _ = testGroup "prop_validity"
  [ testProperty "prop_std_valid" $ \bs ->
    validate @a bs
  , testProperty "prop_url_valid" $ \bs ->
    validateUrl @a bs
  ]

prop_url_padding :: forall a b. Harness a b => a -> TestTree
prop_url_padding _ = testGroup "prop_url_padding"
  [ testProperty "prop_url_nopad_roundtrip" $ \bs ->
      Right bs == decodeUrlNopad @a (encodeUrlNopad @a bs)
  , testProperty "prop_url_pad_roundtrip" $ \bs ->
      Right bs == decodeUrlPad @a (encodeUrl @a bs)
  , testProperty "prop_url_decode_invariant" $ \bs ->
      (Right bs == decodeUrl @a (encodeUrl @a bs)
      || Right bs == decodeUrlNopad @a (encodeUrlNopad @a bs))
      || Right bs == decodeUrlPad @a (encodeUrl @a bs)
  , testProperty "prop_url_padding_coherence" $ \bs ->
      Right bs == decodeUrl @a (encodeUrl @a bs)
      && Right bs == decodeUrlPad @a (encodeUrl @a bs)
  ]


prop_bos_coherence :: TestTree
prop_bos_coherence = testGroup "prop_bos_coherence"
  [ testProperty "prop_std_bos_coherence" $ \bs ->
      Right bs == B64.decodeBase64 (B64.encodeBase64' bs)
      && Right bs == Bos.decode (Bos.encode bs)
  , testProperty "prop_url_bos_coherence" $ \bs ->
      Right bs == B64U.decodeBase64 (B64U.encodeBase64' bs)
      && Right bs == BosU.decode (BosU.encode bs)
  ]

-- ---------------------------------------------------------------- --
-- Unit tests

rfcVectors :: TestTree
rfcVectors = testGroup "RFC 4648 Test Vectors"
    [ testGroup "std alphabet"
      [ testCaseStd "" ""
      , testCaseStd "f" "Zg=="
      , testCaseStd "f" "Zg=="
      , testCaseStd "fo" "Zm8="
      , testCaseStd "foo" "Zm9v"
      , testCaseStd "foob" "Zm9vYg=="
      , testCaseStd "fooba" "Zm9vYmE="
      , testCaseStd "foobar" "Zm9vYmFy"
      ]
    , testGroup "url-safe alphabet"
      [ testCaseUrl "" ""
      , testCaseUrl "<" "PA=="
      , testCaseUrl "<<" "PDw="
      , testCaseUrl "<<?" "PDw_"
      , testCaseUrl "<<??" "PDw_Pw=="
      , testCaseUrl "<<??>" "PDw_Pz4="
      , testCaseUrl "<<??>>" "PDw_Pz4-"
      ]
    ]
  where
    testCaseStd :: BS.ByteString -> BS.ByteString -> TestTree
    testCaseStd s t = testCase (show $ if s == "" then "empty" else s) $ do
      t @=? B64.encodeBase64' s
      Right s @=? B64.decodeBase64 (B64.encodeBase64' s)

    testCaseUrl :: BS.ByteString -> BS.ByteString -> TestTree
    testCaseUrl s t = testCase (show $ if s == "" then "empty" else s) $ do
      t @=? B64U.encodeBase64' s
      Right s @=? B64U.decodeBase64Padded t

paddingTests :: TestTree
paddingTests = testGroup "Padding tests"
    [ testGroup "URL decodePadding coherence"
      [ ptest "<" "PA=="
      , ptest "<<" "PDw="
      , ptest "<<?" "PDw_"
      , ptest "<<??" "PDw_Pw=="
      , ptest "<<??>" "PDw_Pz4="
      , ptest "<<??>>" "PDw_Pz4-"
      ]
    , testGroup "URL decodeUnpadded coherence"
      [ utest "<" "PA"
      , utest "<<" "PDw"
      , utest "<<?" "PDw_"
      , utest "<<??" "PDw_Pw"
      , utest "<<??>" "PDw_Pz4"
      , utest "<<??>>" "PDw_Pz4-"
      ]
    ]
  where
    ptest :: BS.ByteString -> BS.ByteString -> TestTree
    ptest s t =
      testCaseSteps (show $ if t == "" then "empty" else t) $ \step -> do
        let u = B64U.decodeBase64Unpadded t
            v = B64U.decodeBase64Padded t

        if BS.last t == 0x3d then do
          step "Padding required: no padding fails"
          u @=? Left "Base64-encoded bytestring has invalid padding"

          step "Padding required: padding succeeds"
          v @=? Right s
        else do
          step "String has no padding: decodes should coincide"
          u @=? Right s
          v @=? Right s
          v @=? u

    utest :: BS.ByteString -> BS.ByteString -> TestTree
    utest s t =
      testCaseSteps (show $ if t == "" then "empty" else t) $ \step -> do
        let u = B64U.decodeBase64Padded t
            v = B64U.decodeBase64Unpadded t

        if BS.length t `mod` 4 == 0 then do
          step "String has no padding: decodes should coincide"
          u @=? Right s
          v @=? Right s
          v @=? u
        else do
          step "Unpadded required: padding fails"
          u @=? Left "Base64-encoded bytestring requires padding"

          step "Unpadded required: unpadding succeeds"
          v @=? Right s
