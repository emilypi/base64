{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Main
( main
, tests
) where


import "base64" Data.ByteString.Base64 as B64
import "base64" Data.ByteString.Base64.URL as B64U
import "base64-bytestring" Data.ByteString.Base64 as Bos
import Data.ByteString.Random (random)
import Data.Functor (void)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Base64 Tests"
    [ testVectors
    , sanityTests
    ]

testVectors :: TestTree
testVectors = testGroup "RFC 4648 Test Vectors"
    [ testCaseB64 "" ""
    , testCaseB64 "f" "Zg=="
    , testCaseB64 "fo" "Zm8="
    , testCaseB64 "foo" "Zm9v"
    , testCaseB64 "foob" "Zm9vYg=="
    , testCaseB64 "fooba" "Zm9vYmE="
    , testCaseB64 "foobar" "Zm9vYmFy"
    ]
  where
    testCaseB64 s t =
      testCase (T.unpack $ if s == "" then "empty" else s) $
        t @=?  B64.encodeBase64 (T.encodeUtf8 s)

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
      void $ return $ B64.encodeBase64 bs
      -- void $ return $ B64U.encodeBase64 bs

    compare64 n = testCase ("Testing " ++ show n ++ "-sized bytestrings") $ do
      bs <- random n
      B64.encodeBase64 bs @=? Bos.encode bs

    roundtrip n = testCase ("Roundtrip encode/decode for " ++ show n ++ "-sized bytestrings") $ do
      bs <- random n
      B64.decodeBase64 (B64.encodeBase64 bs) @=? Right bs
      -- B64U.decodeBase64 (B64U.encodeBase64 bs) @=? Right bs
