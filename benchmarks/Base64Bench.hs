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


import Gauge.Main

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
      [ bgroup "base64-bytestring"
        [ bench "25" $ whnf Bos.encode bs25
        , bench "100" $ whnf Bos.encode bs100
        , bench "1000" $ whnf Bos.encode bs1k
        , bench "10000" $ whnf Bos.encode bs10k
        , bench "100000" $ whnf Bos.encode bs100k
        , bench "1000000" $ whnf Bos.encode bs1mm
        ]
      , bgroup "base64"
        [ bench "25" $ whnf B64.encodeBase64' bs25
        , bench "100" $ whnf B64.encodeBase64' bs100
        , bench "1000" $ whnf B64.encodeBase64' bs1k
        , bench "10000" $ whnf B64.encodeBase64' bs10k
        , bench "100000" $ whnf B64.encodeBase64' bs100k
        , bench "1000000" $ whnf B64.encodeBase64' bs1mm
        ]
      ]

    , env b64s $ \ ~(bs25,bs100,bs1k,bs10k,bs100k,bs1mm) ->
      bgroup "decode"
      [ bgroup "base64-bytestring"
        [ bench "25" $ whnf Bos.decode bs25
        , bench "100" $ whnf Bos.decode bs100
        , bench "1000" $ whnf Bos.decode bs1k
        , bench "10000" $ whnf Bos.decode bs10k
        , bench "100000" $ whnf Bos.decode bs100k
        , bench "1000000" $ whnf Bos.decode bs1mm
        ]
      , bgroup "base64"
        [ bench "25" $ whnf B64.decodeBase64 bs25
        , bench "100" $ whnf B64.decodeBase64 bs100
        , bench "1000" $ whnf B64.decodeBase64 bs1k
        , bench "10000" $ whnf B64.decodeBase64 bs10k
        , bench "100000" $ whnf B64.decodeBase64 bs100k
        , bench "1000000" $ whnf B64.decodeBase64 bs1mm
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

    b64s = do
      bs' <- bs
      return $ case bs' of
        (a,b,c,d,e,f) ->
          ( B64.encodeBase64' a
          , B64.encodeBase64' b
          , B64.encodeBase64' c
          , B64.encodeBase64' d
          , B64.encodeBase64' e
          , B64.encodeBase64' f
          )
