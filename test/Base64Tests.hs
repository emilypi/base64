{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Main
( main
, tests
) where


import qualified Data.ByteString as BS
import "base64" Data.ByteString.Base64 as B64
import "base64" Data.ByteString.Base64.URL as B64U
import "base64-bytestring" Data.ByteString.Base64 as Bos
import Data.ByteString.Random (random)
import Data.Functor (void)

import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Base64 Tests"
    [ testVectors
    , sanityTests
    , alphabetTests
    ]

testVectors :: TestTree
testVectors = testGroup "RFC 4648 Test Vectors"
    [ testGroup "encode/decode"
      [ testCaseB64 "" ""
      , testCaseB64 "f" "Zg=="
      , testCaseB64 "fo" "Zm8="
      , testCaseB64 "foo" "Zm9v"
      , testCaseB64 "foob" "Zm9vYg=="
      , testCaseB64 "fooba" "Zm9vYmE="
      , testCaseB64 "foobar" "Zm9vYmFy"
      ]
    , testGroup "encode/decode url-safe"
      [ testCaseB64' "" ""
      , testCaseB64' "<" "PA=="
      , testCaseB64' "<<" "PDw="
      , testCaseB64' "<<?" "PDw_"
      , testCaseB64' "<<??" "PDw_Pw=="
      , testCaseB64' "<<??>" "PDw_Pz4="
      , testCaseB64' "<<??>>" "PDw_Pz4-"
      ]
    ]
  where
    testCaseB64 s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        let t' = B64.encodeBase64' s
            s' = B64.decodeBase64 t'

        step "compare encoding w/ padding"
        t @=? t'

        step "compare decoding w/ padding"
        Right s @=? s'

    testCaseB64' s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        let t' = B64U.encodeBase64' s
            s' = B64U.decodeBase64 t'
            u = B64U.encodeBase64Unpadded' s
            v = B64U.decodeBase64 u

        step "compare url-safe encoding w/ padding"
        t @=? t'

        step "compare url-safe decoding w/ padding"
        Right s @=? s'

        step "compare url-safe encoding w/o padding"
        t @=? t'

        step "compare url-safe decoding w/o padding"
        Right s @=? v

sanityTests :: TestTree
sanityTests = testGroup "Sanity tests"
    [ testGroup "very large bytestrings don't segfault"
        [ chonk
        ]
    , testGroup "`base64-bytestring` sanity checks"
        [ compare64 3
        , compare64 4
        , compare64 5
        , compare64 6
        , compare64 1000
        , compare64 100000
        ]
    , testGroup "roundtrip encode/decode"
        [ roundtrip 3
        , roundtrip 4
        , roundtrip 5
        , roundtrip 1000
        , roundtrip 100000
        ]
    ]
  where
    chonk = testCase ("Encoding huge bytestrings doesn't result in OOM or segfault") $ do
      bs <- random 1000000
      void $ return $ B64.encodeBase64' bs
      void $ return $ B64U.encodeBase64' bs

    compare64 n = testCase ("Testing " ++ show n ++ "-sized bytestrings") $ do
      bs <- random n
      B64.encodeBase64' bs @=? Bos.encode bs

    roundtrip n = testCase ("Roundtrip encode/decode for " ++ show n ++ "-sized bytestrings") $ do
      bs <- random n
      B64.decodeBase64 (B64.encodeBase64' bs) @=? Right bs
      B64U.decodeBase64 (B64U.encodeBase64' bs) @=? Right bs

alphabetTests :: TestTree
alphabetTests = testGroup "Alphabet tests"
    [ base64Tests 0
    , base64Tests 4
    , base64Tests 5
    , base64Tests 6
    , base64Tests 100
    , base64UrlTests 0
    , base64UrlTests 4
    , base64UrlTests 5
    , base64UrlTests 6
    , base64UrlTests 100
    ]
  where
    base64Tests n = testCase ("Conforms to Base64 alphabet: " ++ show n) $ do
      bs <- random n
      let b = B64.encodeBase64' bs
      assertBool ("failed validity: " ++ show b) $ B64.isValidBase64 b
      assertBool ("failed correctness: " ++ show b) $ B64.isBase64 b

    base64UrlTests n = testCase ("Conforms to Base64url alphabet: " ++ show n) $ do
      bs <- random n
      let b = B64U.encodeBase64' bs
      assertBool ("failed validity: " ++ show b) $ B64U.isValidBase64Url b
      assertBool ("failed correctness: " ++ show b) $ B64U.isBase64Url b
