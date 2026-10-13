{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports #-}
-- |
-- Module       : Main
-- Copyright    : (c) 2019-2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Build once with -f-simd and once with -fsimd to compare the same cases.
-- Sizes describe raw input bytes; sliced inputs also apply to decode buffers.
module Main (main) where

import Criterion.Main
import "base64" Data.Base64.Types (Alphabet(StdPadded), assertBase64)
import qualified Data.ByteString as BS
import qualified "base64" Data.ByteString.Base64 as B64
import qualified "base64" Data.ByteString.Base64.URL as B64U
import qualified "base64-bytestring" Data.ByteString.Base64 as Bos
import qualified "base64-bytestring" Data.ByteString.Base64.URL as BosU


main :: IO ()
main = defaultMain
  [ bgroup implementation
      [ bgroup (show size)
          [ env (inputs size offset) $ \ ~(raw, std, url, unpadded) ->
              bgroup ("offset-" ++ show offset)
                [ bgroup "standard"
                    [ bgroup "encode"
                        [ bench "base64-bytestring" $ nf Bos.encode raw
                        , bench "base64" $ nf B64.encodeBase64' raw
                        ]
                    , bgroup "decode"
                        [ bench "base64-bytestring" $ nf Bos.decode std
                        , bench "base64-typed" $
                            nf (B64.decodeBase64 . assertBase64 @'StdPadded) std
                        , bench "base64-untyped" $ nf B64.decodeBase64Untyped std
                        ]
                    ]
                , bgroup "url-padded"
                    [ bgroup "encode"
                        [ bench "base64-bytestring" $ nf BosU.encode raw
                        , bench "base64" $ nf B64U.encodeBase64' raw
                        ]
                    , bgroup "decode"
                        [ bench "base64-bytestring" $ nf BosU.decodePadded url
                        , bench "base64-typed" $
                            nf (B64U.decodeBase64Padded . assertBase64) url
                        , bench "base64-untyped" $ nf B64U.decodeBase64PaddedUntyped url
                        ]
                    ]
                , bgroup "url-unpadded"
                    [ bgroup "encode"
                        [ bench "base64-bytestring" $ nf BosU.encodeUnpadded raw
                        , bench "base64" $ nf B64U.encodeBase64Unpadded' raw
                        ]
                    , bgroup "decode"
                        [ bench "base64-bytestring" $ nf BosU.decodeUnpadded unpadded
                        , bench "base64-typed" $
                            nf (B64U.decodeBase64Unpadded . assertBase64) unpadded
                        , bench "base64-untyped" $
                            nf B64U.decodeBase64UnpaddedUntyped unpadded
                        ]
                    ]
                ]
          | offset <- [0, 1]
          ]
      | size <- [11, 12, 24, 32, 48, 64, 128, 4096, 1048576]
      ]
  ]

implementation :: String
#ifdef BASE64_SIMD
implementation = "simd"
#else
implementation = "scalar"
#endif

-- Criterion forces this environment before timing. The exact same deterministic
-- data is used for both flag settings, and encoding is excluded from decoding
-- measurements. Reference encodings supply the known-valid typed inputs.
inputs :: Int -> Int -> IO (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString)
inputs size offset = pure
  ( sliced raw
  , sliced (Bos.encode raw)
  , sliced (BosU.encode raw)
  , sliced (BosU.encodeUnpadded raw)
  )
  where
    raw = BS.pack (take size (cycle [0 .. 255]))
    sliced bytes = BS.take (BS.length bytes) . BS.drop offset $
      BS.replicate offset 0xe5 <> bytes <> BS.replicate 32 0xff
