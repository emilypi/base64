{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Main
( main
, tests
) where


import "base64" Data.ByteString.Base64 as B64
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
    [ testCase "very large bytestrings don't segfault" chonk
    , testGroup "`base64-bytestring` sanity checks"
        [ compare64 3
        , compare64 4
        , compare64 5
        ]
    ]
  where
    chonk =
      -- if no OOM or segfault, we good
      void $ encodeBase64 <$> random 1000000

    compare64 n = testCase ("Testing size " ++ show n ++ " bytestrings") $ do
      bs <- random n
      B64.encodeBase64 bs @=? Bos.encode bs
