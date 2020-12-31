{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
) where


import Prelude hiding (length)

import Data.Bifunctor (second)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import "base64" Data.ByteString.Base64 as B64
import "base64" Data.ByteString.Base64.URL as B64U
import qualified "base64-bytestring" Data.ByteString.Base64 as Bos
import qualified "base64-bytestring" Data.ByteString.Base64.URL as BosU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Base64.Error (Base64Error(..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Short as TS
import Data.Word

import Internal

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Data.String (IsString)
import Test.QuickCheck hiding (label)


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Base64 Tests"
  [ mkTree b64
    [ mkPropTree
    , mkUnitTree BS.last BS.length
    ]
  , mkTree lb64
    [ mkPropTree
    , mkUnitTree LBS.last (fromIntegral . LBS.length)
    ]
  , mkTree sb64
    [ mkPropTree
    , mkUnitTree (BS.last . SBS.fromShort) SBS.length
    ]
  , mkTree t64
    [ mkPropTree
    , mkUnitTree (c2w . T.last) T.length
    , mkDecodeTree T.decodeUtf8' tt64 b64
    ]
  , mkTree tl64
    [ mkPropTree
    , mkUnitTree (c2w . TL.last) (fromIntegral . TL.length)
    , mkDecodeTree TL.decodeUtf8' ttl64 lb64
    ]
  , mkTree ts64
    [ mkPropTree
    , mkUnitTree (c2w . T.last . TS.toText) TS.length
    , mkDecodeTree
      (second TS.fromText . T.decodeUtf8' . SBS.fromShort) tts64 sb64
    ]
  ]

-- ---------------------------------------------------------------- --
-- Test tree generation

-- | Make a test tree for a given label
--
mkTree
  :: ( Arbitrary a
     , IsString a
     , Eq a
     , Show a
     )
  => Harness a
  -> [Harness a -> TestTree]
  -> TestTree
mkTree a = testGroup (label a) . fmap ($ a)

-- | Make a test group with some name, lifting a test tree up to the correct
-- type information via some Harness
--
mkTests
  :: ( Arbitrary a
     , IsString a
     , Eq a
     , Show a
     )
  => String
  -> [Harness a -> TestTree]
  -> Harness a
  -> TestTree
mkTests context ts = testGroup context . (<*>) ts . pure

-- | Make property tests for a given harness instance
--
mkPropTree :: (Arbitrary a, IsString a, Eq a, Show a) => Harness a -> TestTree
mkPropTree = mkTests "Property Tests"
  [ prop_roundtrip
  , prop_correctness
  , prop_url_padding
  , const prop_bos_coherence
  ]

-- | Make unit tests for a given harness instance
--
mkUnitTree
  :: (Arbitrary a, IsString a, Eq a, Show a)
  => (a -> Word8)
  -> (a -> Int)
  -> Harness a
  -> TestTree
mkUnitTree last_ length_ = mkTests "Unit tests"
  [ paddingTests last_ length_
  , rfcVectors
  , offsetVectors
  , validityTests
  , canonicityTests
  ]

-- | Make unit tests for textual 'decode*With' functions
--
mkDecodeTree
  :: ( Arbitrary t
     , Eq t
     , IsString t
     , Show t
     , IsString a
     , Show e
     )
  => (a -> Either e t)
  -> TextHarness a t
  -> Harness a
  -> Harness t
  -> TestTree
mkDecodeTree utf8 t a = mkTests "Decoding tests"
  [ decodeWithVectors utf8 t a
  ]

-- ---------------------------------------------------------------- --
-- Property tests

prop_roundtrip :: (Arbitrary a, IsString a, Eq a, Show a) => Harness a -> TestTree
prop_roundtrip Harness{..} = testGroup "prop_roundtrip"
  [ testProperty "prop_std_roundtrip" $ \(bs :: b) ->
      Right (encode bs) == decode (encode (encode bs))
  , testProperty "prop_url_roundtrip" $ \(bs :: b) ->
      Right (encodeUrl bs) == decodeUrl (encodeUrl (encodeUrl bs))
  , testProperty "prop_url_roundtrip_nopad" $ \(bs :: b) ->
      Right (encodeUrlNopad bs)
        == decodeUrlNopad (encodeUrlNopad (encodeUrlNopad bs))
  , testProperty "prop_std_lenient_roundtrip" $ \(bs :: b) ->
      encode bs == lenient (encode (encode bs))
  , testProperty "prop_url_lenient_roundtrip" $ \(bs :: b) ->
      encodeUrl bs == lenientUrl (encodeUrl (encodeUrl bs))
  ]

prop_correctness :: (Arbitrary a, IsString a, Eq a, Show a) => Harness a -> TestTree
prop_correctness Harness{..} = testGroup "prop_validity"
  [ testProperty "prop_std_valid" $ \(bs :: b) ->
    validate (encode bs)
  , testProperty "prop_url_valid" $ \(bs :: b) ->
    validateUrl (encodeUrl bs)
  , testProperty "prop_std_correct" $ \(bs :: b) ->
    correct (encode bs)
  , testProperty "prop_url_correct" $ \(bs :: b) ->
    correctUrl (encodeUrl bs)
  ]

prop_url_padding :: (Arbitrary a, IsString a, Eq a, Show a) => Harness a -> TestTree
prop_url_padding Harness{..}  = testGroup "prop_url_padding"
  [ testProperty "prop_url_nopad_roundtrip" $ \(bs :: b) ->
      Right (encodeUrlNopad bs)
        == decodeUrlNopad (encodeUrlNopad (encodeUrlNopad bs))

  , testProperty "prop_url_pad_roundtrip" $ \(bs :: b) ->
      Right (encodeUrl bs) == decodeUrlPad (encodeUrl (encodeUrl bs))

  , testProperty "prop_url_decode_invariant" $ \(bs :: b) ->
      ( decodeUrlNopad (encodeUrlNopad (encodeUrlNopad bs))
      == decodeUrl (encodeUrl (encodeUrl bs))
      ) ||
      ( decodeUrlPad (encodeUrl (encodeUrl bs))
      == decodeUrl (encodeUrl (encodeUrl bs))
      )

  -- NOTE: we need to fix the bitmasking issue for "impossible"
  -- inputs

  , testProperty "prop_url_padding_coherence" $ \(bs :: b) ->
      Right (encodeUrl bs) == decodeUrl (encodeUrl (encodeUrl bs))
      && Right (encodeUrl bs) == decodeUrlPad (encodeUrl (encodeUrl bs))

  , testProperty "prop_url_nopadding_coherence" $ \(bs :: b) ->
      Right (encodeUrlNopad bs) == decodeUrlNopad (encodeUrlNopad (encodeUrlNopad bs))
      && Right (encodeUrlNopad bs) == decodeUrl (encodeUrlNopad (encodeUrlNopad bs))
  ]

-- | just a sanity check against `base64-bytestring`
--
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

-- | RFC 4648 test vectors
--
rfcVectors :: (IsString a, Eq a, Show a) => Harness a -> TestTree
rfcVectors Harness{..} = testGroup "RFC 4648 Test Vectors"
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
    testCaseStd s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        step "encode is sound"
        t @=? encode s

        step "decode is sound"
        Right s @=? decode (encode s)

    testCaseUrl s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        step "encode is sound"
        t @=? encodeUrl s

        step "decode is sound"
        Right s @=? decodeUrlPad t

-- | Url-safe padding unit tests (stresses entire alphabet)
--
paddingTests
  :: ( IsString a
     , Eq a
     , Show a
     )
  => (a -> Word8)
  -> (a -> Int)
  -> Harness a
  -> TestTree
paddingTests last_ length_ Harness{..} = testGroup "Padding tests"
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
    , testGroup "url-safe padding case unit tests"
      [ testCase "stress arbitarily padded URL strings" $ do
        decodeUrl "P" @=? Left "Base64-encoded bytestring has invalid size"
        decodeUrl "PA" @=? Right "<"
        decodeUrl "PDw" @=? Right "<<"
        decodeUrl "PDw_" @=? Right "<<?"
      , testCase "stress padded URL strings" $ do
        decodeUrlPad "=" @=? Left "Base64-encoded bytestring has invalid size"
        decodeUrlPad "PA==" @=? Right "<"
        decodeUrlPad "PDw=" @=? Right "<<"
        decodeUrlPad "PDw_" @=? Right "<<?"
      , testCase "stress unpadded URL strings" $ do
        decodeUrlNopad "P" @=? Left "Base64-encoded bytestring has invalid size"
        decodeUrlNopad "PA" @=? Right "<"
        decodeUrlNopad "PDw" @=? Right "<<"
        decodeUrlNopad "PDw_" @=? Right "<<?"
      ]
    ]
  where
    ptest s t =
      testCaseSteps (show $ if t == "" then "empty" else t) $ \step -> do
        let u = decodeUrlNopad t
            v = decodeUrlPad t

        if last_ t == 0x3d then do
          step "Padding required: no padding fails"
          u @=? Left "Base64-encoded bytestring has invalid padding"

          step "Padding required: padding succeeds"
          v @=? Right s
        else do
          step "String has no padding: decodes should coincide"
          u @=? Right s
          v @=? Right s
          v @=? u

    utest s t =
      testCaseSteps (show $ if t == "" then "empty" else t) $ \step -> do
        let u = decodeUrlPad t
            v = decodeUrlNopad t

        if length_ t `mod` 4 == 0 then do
          step "String has no padding: decodes should coincide"
          u @=? Right s
          v @=? Right s
          v @=? u
        else do
          step "Unpadded required: padding fails"
          u @=? Left "Base64-encoded bytestring requires padding"

          step "Unpadded required: unpadding succeeds"
          v @=? Right s

-- | Offset test vectors. This stresses the invalid char + incorrect padding
-- offset error messages
--
offsetVectors :: (IsString a, Eq a, Show a) => Harness a -> TestTree
offsetVectors Harness{..} = testGroup "Offset tests"
  [ testGroup "Invalid padding"
    [ testCase "Invalid staggered padding" $ do
      decodeUrl "=A==" @=? Left "invalid padding at offset: 0"
      decodeUrl "P===" @=? Left "invalid padding at offset: 1"
    , testCase "Invalid character coverage - final chunk" $ do
      decodeUrl "%D==" @=? Left "invalid character at offset: 0"
      decodeUrl "P%==" @=? Left "invalid character at offset: 1"
      decodeUrl "PD%=" @=? Left "invalid character at offset: 2"
      decodeUrl "PA=%" @=? Left "invalid character at offset: 3"
      decodeUrl "PDw%" @=? Left "invalid character at offset: 3"
    , testCase "Invalid character coverage - decode chunk" $ do
      decodeUrl "%Dw_PDw_" @=? Left "invalid character at offset: 0"
      decodeUrl "P%w_PDw_" @=? Left "invalid character at offset: 1"
      decodeUrl "PD%_PDw_" @=? Left "invalid character at offset: 2"
      decodeUrl "PDw%PDw_" @=? Left "invalid character at offset: 3"
    , testCase "Invalid padding in body" $ do
      decodeUrl "PD=_PDw_" @=? Left "invalid padding at offset: 2"
      decodeUrl "PDw=PDw_" @=? Left "invalid padding at offset: 3"
    , testCase "Padding fails everywhere but end" $ do
      decode "=eAoeAo=" @=? Left "invalid padding at offset: 0"
      decode "e=AoeAo=" @=? Left "invalid padding at offset: 1"
      decode "eA=oeAo=" @=? Left "invalid padding at offset: 2"
      decode "eAo=eAo=" @=? Left "invalid padding at offset: 3"
      decode "eAoe=Ao=" @=? Left "invalid padding at offset: 4"
      decode "eAoeA=o=" @=? Left "invalid padding at offset: 5"
    ]
  ]

canonicityTests :: (IsString a, Eq a, Show a) => Harness a -> TestTree
canonicityTests Harness{..} =  testGroup "Canonicity unit tests"
    [ testCase "roundtrip for d ~ ZA==" $ do
      decode "ZE==" @=? Left "non-canonical encoding detected at offset: 1"
      decode "ZK==" @=? Left "non-canonical encoding detected at offset: 1"
      decode "ZA==" @=? Right "d"
    , testCase "roundtrip for f` ~ ZmA=" $ do
      decode "ZmC=" @=? Left "non-canonical encoding detected at offset: 2"
      decode "ZmD=" @=? Left "non-canonical encoding detected at offset: 2"
      decode "ZmA=" @=? Right "f`"

    , testCase "roundtrip for foo` ~ Zm9vYA==" $ do
      decode "Zm9vYE==" @=? Left "non-canonical encoding detected at offset: 5"
      decode "Zm9vYK==" @=? Left "non-canonical encoding detected at offset: 5"
      decode "Zm9vYA==" @=? Right "foo`"

    , testCase "roundtrip for foob` ~ Zm9vYmA=" $ do
      decode "Zm9vYmC=" @=? Left "non-canonical encoding detected at offset: 6"
      decode "Zm9vYmD=" @=? Left "non-canonical encoding detected at offset: 6"
      decode "Zm9vYmA=" @=? Right "foob`"
    ]

-- | Unit test trees for the `decode*With` family of text-valued functions
--
decodeWithVectors
  :: ( IsString a
     , IsString t
     , Eq t
     , Show e
     , Show t
     )
  => (a -> Either e t)
    -- ^ utf8
  -> TextHarness a t
    -- ^ witness to the bytestring-ey dictionaries
  -> Harness a
    -- ^ witness to the text dictionaries
  -> Harness t
  -> TestTree
decodeWithVectors utf8 TextHarness{..} h t = testGroup "DecodeWith* unit tests"
  [ testGroup "decodeWith negative tests"
    [ testCase "decodeWith non-utf8 inputs on decodeUtf8" $ do
      case decodeWith_ utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeWith valid utf8 inputs on decodeUtf8" $ do
      case decodeWith_ utf8 (encode h "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeUrlWith non-utf8 inputs on decodeUtf8" $ do
      case decodeUrlWith_ utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeUrlWith valid utf8 inputs on decodeUtf8" $ do
      case decodeUrlWith_ utf8 (encodeUrl h "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeUrlPaddedWith non-utf8 inputs on decodeUtf8" $ do
      case decodeUrlPaddedWith_ utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodePaddedWith valid utf8 inputs on decodeUtf8" $ do
      case decodeUrlPaddedWith_ utf8 (encodeUrl h "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeUnpaddedWith non-utf8 inputs on decodeUtf8" $ do
      case decodeUrlUnpaddedWith_ utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeUnpaddedWith valid utf8 inputs on decodeUtf8" $ do
      case decodeUrlUnpaddedWith_ utf8 (encodeUrlNopad h "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    ]
  , testGroup "decodeWith positive tests"
    [ testCase "decodeWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decode t "Zm9vYmFy"
      b <- either (assertFailure . show) pure $ decodeWith_ utf8 "Zm9vYmFy"
      a @=? b
    , testCase "decodeUrlWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodeUrl t "PDw_Pz4-"
      b <- either (assertFailure . show) pure $ decodeUrlWith_ utf8 "PDw_Pz4-"
      a @=? b
    , testCase "decodeUrlPaddedWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodeUrlPad t "PDw_Pz4-"
      b <- either (assertFailure . show) pure $ decodeUrlPaddedWith_ utf8 "PDw_Pz4-"
      a @=? b
    , testCase "decodeUrlUnpaddedWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodeUrlNopad t "PDw_Pz4-"
      b <- either (assertFailure . show) pure $ decodeUrlUnpaddedWith_ utf8 "PDw_Pz4-"
      a @=? b
    ]
  ]

-- | Validity unit tests for the URL workflow
--
validityTests :: IsString a => Harness a -> TestTree
validityTests Harness{..} = testGroup "Validity and correctness unit tests"
  [ testGroup "Validity unit tests"
    [ testCase "Padding tests" $ do
      not (validateUrl "P") @? "P"
      validateUrl "PA" @? "PA"
      validateUrl "PDw" @? "PDw"
      validateUrl "PDw_" @? "PDw_"
      validateUrl "PA==" @? "PA=="
      validateUrl "PDw=" @? "PDw="
      validateUrl "PDw_" @? "PDw_"

    , testCase "Canonicity tests" $ do
      validateUrl "ZK==" @? "ZK=="
      validateUrl "ZE==" @? "ZE=="

      validateUrl "ZA==" @? "ZA=="
      validateUrl "ZK==" @? "ZK=="
      validateUrl "ZK" @? "ZK"

      validateUrl "ZmA=" @? "ZmA="
      validateUrl "ZmC=" @? "ZmC="
      validateUrl "ZmE" @? "ZmE"

      validateUrl "Zm9vYmA=" @? "Zm9vYmA="
      validateUrl "Zm9vYmC=" @? "Zm9vYmC="
      validateUrl "Zm9vYmC" @? "Zm9vYmC"
    ]
  , testGroup "Correctness unit tests"
    [ testCase "Padding tests" $ do
      not (validateUrl "P") @? "P"
      correctUrl "PA" @? "PA"
      correctUrl "PDw" @? "PDw"
      correctUrl "PDw_" @? "PDw_"
      correctUrl "PA==" @? "PA=="
      correctUrl "PDw=" @? "PDw="
      correctUrl "PDw_" @? "PDw_"

    , testCase "Canonicity tests" $ do
      not (correctUrl "ZK==") @? "ZK=="
      not (correctUrl "ZE==") @? "ZE=="
      correctUrl "ZA==" @? "ZA=="

      correctUrl "ZmA=" @? "ZmA="
      not (correctUrl "ZmC=") @? "ZmC="
      not (correctUrl "ZmD") @? "ZmD"

      correctUrl "Zm9vYmA=" @? "Zm9vYmA="
      not (correctUrl "Zm9vYmC=") @? "Zm9vYmC="
      not (correctUrl "Zm9vYmC") @? "Zm9vYmC"
    ]
  ]
