{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Main
( main
) where


import Criterion
import Criterion.Main

import "memory" Data.ByteArray.Encoding as Mem
import Data.ByteString
import "base64-bytestring" Data.ByteString.Base64 as Bos
import "base64" Data.ByteString.Base64 as B64
import Data.ByteString.Random (random)


main :: IO ()
main =
  defaultMain
    [ env bs $ \ ~(bs25,bs100,bs1k,bs10k,bs100k,bs1mm) ->
      bgroup "encode"
      [ bgroup "25"
        [ bench "memory" $ whnf ctob bs25
        , bench "base64-bytestring" $ whnf Bos.encode bs25
        , bench "base64" $ whnf B64.encodeBase64' bs25
        ]
      , bgroup "100"
        [ bench "memory" $ whnf ctob bs100
        , bench "base64-bytestring" $ whnf Bos.encode bs100
        , bench "base64" $ whnf B64.encodeBase64' bs100
        ]
      , bgroup "1k"
        [ bench "memory" $ whnf ctob bs1k
        , bench "base64-bytestring" $ whnf Bos.encode bs1k
        , bench "base64" $ whnf B64.encodeBase64' bs1k
        ]
      , bgroup "10k"
        [ bench "memory" $ whnf ctob bs10k
        , bench "base64-bytestring" $ whnf Bos.encode bs10k
        , bench "base64" $ whnf B64.encodeBase64' bs10k
        ]
      , bgroup "100k"
        [ bench "memory" $ whnf ctob bs100k
        , bench "base64-bytestring" $ whnf Bos.encode bs100k
        , bench "base64" $ whnf B64.encodeBase64' bs100k
        ]
      , bgroup "1mm"
        [ bench "memory" $ whnf ctob bs1mm
        , bench "base64-bytestring" $ whnf Bos.encode bs1mm
        , bench "base64" $ whnf B64.encodeBase64' bs1mm
        ]
      ]
    , env bs' $ \ ~(bs25,bs100,bs1k,bs10k,bs100k,bs1mm) ->
      bgroup "decode"
      [ bgroup "25"
        [ bench "memory" $ whnf btoc bs25
        , bench "base64-bytestring" $ whnf Bos.decode bs25
        , bench "base64" $ whnf B64.decodeBase64 bs25
        ]
      , bgroup "100"
        [ bench "memory" $ whnf btoc bs100
        , bench "base64-bytestring" $ whnf Bos.decode bs100
        , bench "base64" $ whnf B64.decodeBase64 bs100
        ]
      , bgroup "1k"
        [ bench "memory" $ whnf btoc bs1k
        , bench "base64-bytestring" $ whnf Bos.decode bs1k
        , bench "base64" $ whnf B64.decodeBase64 bs1k
        ]
      , bgroup "10k"
        [ bench "memory" $ whnf btoc bs10k
        , bench "base64-bytestring" $ whnf Bos.decode bs10k
        , bench "base64" $ whnf B64.decodeBase64 bs10k
        ]
      , bgroup "100k"
        [ bench "memory" $ whnf btoc bs100k
        , bench "base64-bytestring" $ whnf Bos.decode bs100k
        , bench "base64" $ whnf B64.decodeBase64 bs100k
        ]
      , bgroup "1mm"
        [ bench "memory" $ whnf btoc bs1mm
        , bench "base64-bytestring" $ whnf Bos.decode bs1mm
        , bench "base64" $ whnf B64.decodeBase64 bs1mm
        ]
      ]
    ]
  where
    ctob :: ByteString -> ByteString
    ctob = Mem.convertToBase Mem.Base64

    btoc :: ByteString -> Either String ByteString
    btoc = Mem.convertFromBase Mem.Base64

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
