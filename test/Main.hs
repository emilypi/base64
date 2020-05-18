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
import Data.Proxy
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


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Base64 Tests"
  [ mkTree (Proxy :: Proxy B64)
    [ mkPropTree
    , mkUnitTree BS.last BS.length
    ]
  , mkTree (Proxy :: Proxy LB64)
    [ mkPropTree
    , mkUnitTree LBS.last (fromIntegral . LBS.length)
    ]
  , mkTree (Proxy :: Proxy SB64)
    [ mkPropTree
    , mkUnitTree (BS.last . SBS.fromShort) SBS.length
    ]
  , mkTree (Proxy :: Proxy T64)
    [ mkPropTree
    , mkUnitTree (c2w . T.last) T.length
    , mkDecodeTree T.decodeUtf8' (Proxy :: Proxy B64)
    ]
  , mkTree (Proxy :: Proxy TL64)
    [ mkPropTree
    , mkUnitTree (c2w . TL.last) (fromIntegral . TL.length)
    , mkDecodeTree TL.decodeUtf8' (Proxy :: Proxy LB64)
    ]
  , mkTree (Proxy :: Proxy TS64)
    [ mkPropTree
    , mkUnitTree (c2w . T.last . TS.toText) TS.length
    , mkDecodeTree
      (second TS.fromText . T.decodeUtf8' . SBS.fromShort)  (Proxy :: Proxy SB64)
    ]
  ]

-- ---------------------------------------------------------------- --
-- Test tree generation

-- | Make a test tree for a given label
--
mkTree
  :: forall a b proxy
  . Harness a b
  => proxy a
  -> [proxy a -> TestTree]
  -> TestTree
mkTree a = testGroup (label @a) . fmap ($ a)

-- | Make a test group with some name, lifting a test tree up to the correct
-- type information via some Harness
--
mkTests
  :: forall a b proxy
  . Harness a b
  => String
  -> [proxy a -> TestTree]
  -> proxy a
  -> TestTree
mkTests context ts = testGroup context . (<*>) ts . pure

-- | Make property tests for a given harness instance
--
mkPropTree :: forall a b proxy. Harness a b => proxy a -> TestTree
mkPropTree = mkTests "Property Tests"
  [ prop_roundtrip
  , prop_correctness
  , prop_url_padding
  , const prop_bos_coherence
  ]

-- | Make unit tests for a given harness instance
--
mkUnitTree
  :: forall a b proxy
  . Harness a b
  => (b -> Word8)
  -> (b -> Int)
  -> proxy a
  -> TestTree
mkUnitTree last_ length_ = mkTests "Unit tests"
  [ paddingTests last_ length_
  , rfcVectors
  , offsetVectors
  ]

-- | Make unit tests for textual 'decode*With' functions
--
mkDecodeTree
  :: forall t a b c e proxy
  . ( TextHarness a b c
    , Harness t c
    , Show e
    )
  => (c -> Either e b)
  -> proxy t
  -> proxy a
  -> TestTree
mkDecodeTree utf8 t = mkTests "Decoding tests"
  [ decodeWithVectors utf8 t
  ]

-- ---------------------------------------------------------------- --
-- Property tests

prop_roundtrip :: forall a b proxy. Harness a b => proxy a -> TestTree
prop_roundtrip _ = testGroup "prop_roundtrip"
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

prop_correctness :: forall a b proxy. Harness a b => proxy a -> TestTree
prop_correctness _ = testGroup "prop_validity"
  [ testProperty "prop_std_valid" $ \(bs :: b) ->
    validate (encode bs)
  , testProperty "prop_url_valid" $ \(bs :: b) ->
    validateUrl (encodeUrl bs)
  , testProperty "prop_std_correct" $ \(bs :: b) ->
    correct (encode bs)
  , testProperty "prop_url_correct" $ \(bs :: b) ->
    correctUrl (encodeUrl bs)
  ]

prop_url_padding :: forall a b proxy. Harness a b => proxy a -> TestTree
prop_url_padding _ = testGroup "prop_url_padding"
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
rfcVectors :: forall a b proxy. Harness a b => proxy a -> TestTree
rfcVectors _ = testGroup "RFC 4648 Test Vectors"
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
        t @=? encode @a s

        step "decode is sound"
        Right s @=? decode (encode s)

    testCaseUrl s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        step "encode is sound"
        t @=? encodeUrl @a s

        step "decode is sound"
        Right s @=? decodeUrlPad t

-- | Url-safe padding unit tests (stresses entire alphabet)
--
paddingTests
  :: forall a b proxy
  . Harness a b
  => (b -> Word8)
  -> (b -> Int)
  -> proxy a
  -> TestTree
paddingTests last_ length_ _ = testGroup "Padding tests"
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
        decodeUrl @a "P" @=? Left "Base64-encoded bytestring has invalid size"
        decodeUrl @a "PA" @=? Right "<"
        decodeUrl @a "PDw" @=? Right "<<"
        decodeUrl @a "PDw_" @=? Right "<<?"
      , testCase "stress padded URL strings" $ do
        decodeUrlPad @a "=" @=? Left "Base64-encoded bytestring has invalid size"
        decodeUrlPad @a "PA==" @=? Right "<"
        decodeUrlPad @a "PDw=" @=? Right "<<"
        decodeUrlPad @a "PDw_" @=? Right "<<?"
      , testCase "stress unpadded URL strings" $ do
        decodeUrlNopad @a "P" @=? Left "Base64-encoded bytestring has invalid size"
        decodeUrlNopad @a "PA" @=? Right "<"
        decodeUrlNopad @a "PDw" @=? Right "<<"
        decodeUrlNopad @a "PDw_" @=? Right "<<?"
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
offsetVectors :: forall a b proxy. Harness a b => proxy a -> TestTree
offsetVectors _ = testGroup "Offset tests"
  [ testCase "Invalid staggered padding" $ do
    decodeUrl @a "=A==" @=? Left "invalid padding at offset: 0"
    decodeUrl @a "P===" @=? Left "invalid padding at offset: 1"
  , testCase "Invalid character coverage - final chunk" $ do
    decodeUrl @a "%D==" @=? Left "invalid character at offset: 0"
    decodeUrl @a "P%==" @=? Left "invalid character at offset: 1"
    decodeUrl @a "PD%=" @=? Left "invalid character at offset: 2"
    decodeUrl @a "PA=%" @=? Left "invalid character at offset: 3"
    decodeUrl @a "PDw%" @=? Left "invalid character at offset: 3"
  , testCase "Invalid character coverage - decode chunk" $ do
    decodeUrl @a "%Dw_PDw_" @=? Left "invalid character at offset: 0"
    decodeUrl @a "P%w_PDw_" @=? Left "invalid character at offset: 1"
    decodeUrl @a "PD%_PDw_" @=? Left "invalid character at offset: 2"
    decodeUrl @a "PDw%PDw_" @=? Left "invalid character at offset: 3"
  , testCase "Invalid padding in body" $ do
    decodeUrl @a "PD=_PDw_" @=? Left "invalid padding at offset: 2"
    decodeUrl @a "PDw=PDw_" @=? Left "invalid padding at offset: 3"
  , testCase "Padding fails everywhere but end" $ do
    decode @a "=eAoeAo=" @=? Left "invalid padding at offset: 0"
    decode @a "e=AoeAo=" @=? Left "invalid padding at offset: 1"
    decode @a "eA=oeAo=" @=? Left "invalid padding at offset: 2"
    decode @a "eAo=eAo=" @=? Left "invalid padding at offset: 3"
    decode @a "eAoe=Ao=" @=? Left "invalid padding at offset: 4"
    decode @a "eAoeA=o=" @=? Left "invalid padding at offset: 5"
  ]

-- | Unit test trees for the `decode*With` family of text-valued functions
--
decodeWithVectors
  :: forall t a b c e proxy
  . ( TextHarness a c b
    , Harness t b
    , Show e
    )
  => (b -> Either e c)
    -- ^ utf8
  -> proxy t
    -- ^ witness to the bytestring-ey dictionaries
  -> proxy a
    -- ^ witness to the text dictionaries
  -> TestTree
decodeWithVectors utf8 _ _ = testGroup "DecodeWith* unit tests"
  [ testGroup "decodeWith negative tests"
    [ testCase "decodeWith non-utf8 inputs on decodeUtf8" $ do
      case decodeWith_ @a utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeWith valid utf8 inputs on decodeUtf8" $ do
      case decodeWith_ @a utf8 (encode @t "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeUrlWith non-utf8 inputs on decodeUtf8" $ do
      case decodeUrlWith_ @a utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeUrlWith valid utf8 inputs on decodeUtf8" $ do
      case decodeUrlWith_ @a utf8 (encodeUrl @t "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeUrlPaddedWith non-utf8 inputs on decodeUtf8" $ do
      case decodeUrlPaddedWith_ @a utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodePaddedWith valid utf8 inputs on decodeUtf8" $ do
      case decodeUrlPaddedWith_ @a utf8 (encodeUrl @t "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeUnpaddedWith non-utf8 inputs on decodeUtf8" $ do
      case decodeUrlUnpaddedWith_ @a utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeUnpaddedWith valid utf8 inputs on decodeUtf8" $ do
      case decodeUrlUnpaddedWith_ @a utf8 (encodeUrlNopad @t "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    ]
  , testGroup "decodeWith positive tests"
    [ testCase "decodeWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decode @a "Zm9vYmFy"
      b <- either (assertFailure . show) pure $ decodeWith_ @a utf8 "Zm9vYmFy"
      a @=? b
    , testCase "decodeUrlWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodeUrl @a "PDw_Pz4-"
      b <- either (assertFailure . show) pure $ decodeUrlWith_ @a utf8 "PDw_Pz4-"
      a @=? b
    , testCase "decodeUrlPaddedWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodeUrlPad @a "PDw_Pz4-"
      b <- either (assertFailure . show) pure $ decodeUrlPaddedWith_ @a utf8 "PDw_Pz4-"
      a @=? b
    , testCase "decodeUrlUnpaddedWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodeUrlNopad @a "PDw_Pz4-"
      b <- either (assertFailure . show) pure $ decodeUrlUnpaddedWith_ @a utf8 "PDw_Pz4-"
      a @=? b
    ]
  ]
