{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Main
( main
) where


import Criterion
import Criterion.Main

import "base64-bytestring" Data.ByteString.Base64 as Bos
import "base64" Data.ByteString.Base64 as B64
import "base64" Data.Base64.Types as B64
import Data.ByteString.Random (random)


main :: IO ()
main =
  defaultMain
    [ env bs $ \ ~(bs25,bs100,bs1k,bs10k,bs100k,bs1mm) ->
      bgroup "encode"
      [ bgroup "25"
        [ bench "base64-bytestring" $ whnf Bos.encode bs25
        , bench "base64" $ whnf B64.encodeBase64' bs25
        ]
      , bgroup "100"
        [ bench "base64-bytestring" $ whnf Bos.encode bs100
        , bench "base64" $ whnf B64.encodeBase64' bs100
        ]
      , bgroup "1k"
        [ bench "base64-bytestring" $ whnf Bos.encode bs1k
        , bench "base64" $ whnf B64.encodeBase64' bs1k
        ]
      , bgroup "10k"
        [ bench "base64-bytestring" $ whnf Bos.encode bs10k
        , bench "base64" $ whnf B64.encodeBase64' bs10k
        ]
      , bgroup "100k"
        [ bench "base64-bytestring" $ whnf Bos.encode bs100k
        , bench "base64" $ whnf B64.encodeBase64' bs100k
        ]
      , bgroup "1mm"
        [ bench "base64-bytestring" $ whnf Bos.encode bs1mm
        , bench "base64" $ whnf B64.encodeBase64' bs1mm
        ]
      ]
    , env bs' $ \ ~(bs25,bs100,bs1k,bs10k,bs100k,bs1mm) ->
      bgroup "decode"
      [ bgroup "25"
        [ bench "base64-bytestring" $ whnf Bos.decode (B64.extractBase64 bs25)
        , bench "base64" $ whnf B64.decodeBase64 bs25
        ]
      , bgroup "100"
        [ bench "base64-bytestring" $ whnf Bos.decode (B64.extractBase64 bs100)
        , bench "base64" $ whnf B64.decodeBase64 bs100
        ]
      , bgroup "1k"
        [ bench "base64-bytestring" $ whnf Bos.decode (B64.extractBase64 bs1k)
        , bench "base64" $ whnf B64.decodeBase64 bs1k
        ]
      , bgroup "10k"
        [ bench "base64-bytestring" $ whnf Bos.decode (B64.extractBase64 bs10k)
        , bench "base64" $ whnf B64.decodeBase64 bs10k
        ]
      , bgroup "100k"
        [ bench "base64-bytestring" $ whnf Bos.decode (B64.extractBase64 bs100k)
        , bench "base64" $ whnf B64.decodeBase64 bs100k
        ]
      , bgroup "1mm"
        [ bench "base64-bytestring" $ whnf Bos.decode (B64.extractBase64 bs1mm)
        , bench "base64" $ whnf B64.decodeBase64 bs1mm
        ]
      ]
    ]
  where
    bs = do
      a <- random 25
      b <- random 100
      c <- random 1000
      d <- random 10000
      e <- random 100000
      f <- random 1000000
      return (a,b,c,d,e,f)

    bs' = do
      a <- B64.encodeBase64' <$> random 25
      b <- B64.encodeBase64' <$> random 100
      c <- B64.encodeBase64' <$> random 1000
      d <- B64.encodeBase64' <$> random 10000
      e <- B64.encodeBase64' <$> random 100000
      f <- B64.encodeBase64' <$> random 1000000
      return (a,b,c,d,e,f)
