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
main = do
  bs25 <- random 25
  bs100 <- random 100
  bs1k <- random 1000
  bs10k <- random 10000
  bs100k <- random 100000

  defaultMain
    [ bgroup "encode"
      [ bgroup "memory"
        [ bench "25" $ whnf ctob bs25
        , bench "100" $ whnf ctob bs100
        , bench "1000" $ whnf ctob bs1k
        , bench "10000" $ whnf ctob bs10k
        , bench "100000" $ whnf ctob bs100k
        ]
      , bgroup "base64-bytestring"
        [ bench "25" $ whnf Bos.encode bs25
        , bench "100" $ whnf Bos.encode bs100
        , bench "1000" $ whnf Bos.encode bs1k
        , bench "10000" $ whnf Bos.encode bs10k
        , bench "100000" $ whnf Bos.encode bs100k
        ]
      , bgroup "base64"
        [ bench "25" $ whnf B64.encodeBase64' bs25
        , bench "100" $ whnf B64.encodeBase64' bs100
        , bench "1000" $ whnf B64.encodeBase64' bs1k
        , bench "10000" $ whnf B64.encodeBase64' bs10k
        , bench "100000" $ whnf B64.encodeBase64' bs100k
        ]
      ]
    ]
  where
    ctob :: ByteString -> ByteString
    ctob = Mem.convertToBase Mem.Base64
