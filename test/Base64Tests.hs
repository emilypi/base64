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
    , miscBase64Tests
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
    testCaseB64 s t = testCase (T.unpack s) $
      t @=?  B64.encodeBase64 (T.encodeUtf8 s)

miscBase64Tests :: TestTree
miscBase64Tests = testGroup "Misc. Base64 Encoding Tests"
    [ testCase "Encoding very large bytestrings" chonk
    , testGroup "`base64-bytestring` sanity checks"
        [ sanity 3
        , sanity 4
        , sanity 5
        ]
    ]
  where
    chonk =
      -- if no OOM or segfault, we good
      void $ encodeBase64 <$> random 1000000

    sanity n = testCase ("Testing size " ++ show n ++ " bytestrings") $ do
      bs <- random n
      B64.encodeBase64 bs @=? Bos.encode bs
