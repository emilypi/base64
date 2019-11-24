{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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


import Control.DeepSeq

import Criterion
import Criterion.Main

import Data.Bifunctor
import "memory" Data.ByteArray.Encoding as Mem
import Data.ByteString
import "base64-bytestring" Data.ByteString.Base64 as Bos
import "base64" Data.ByteString.Base64 as B64
import qualified Data.Text as T

main :: IO ()
main = defaultMain
    [ env globalEnv $ \ ~e -> bgroup "main"
      [ bgroup "base64 encode"
        [ encodeBench @Mem e
        , encodeBench @Bos e
        , encodeBench @B64 e
        ]
      , bgroup "base64 decode"
        [ decodeBench @Mem e
        , decodeBench @Bos e
        , decodeBench @B64 e
        ]
      ]
    ]


globalEnv :: IO ByteString
globalEnv = undefined

encodeBench :: forall a. Impl a => ByteString -> Benchmark
encodeBench = bench (label @a) . nf (encoder @a)

decodeBench :: forall a. Impl a => ByteString -> Benchmark
decodeBench = bench (label @a) . nf (decoder @a)

class (NFData (Base64 a)) => Impl a where
    type Base64 a

    label :: String
    encoder :: ByteString -> Base64 a
    decoder :: ByteString -> Either String (Base64 a)


data Mem

instance Impl Mem where
    type Base64 Mem = ByteString

    label = "memory"
    encoder = Mem.convertToBase Mem.Base64
    decoder = Mem.convertFromBase Mem.Base64

data Bos

instance Impl Bos where
    type Base64 Bos = ByteString

    label = "base64-bytestring"
    encoder = Bos.encode
    decoder = Bos.decode

data B64

instance Impl B64 where
    type Base64 B64 = ByteString

    label = "base64"
    encoder = B64.encodeBase64
    decoder = first T.unpack . B64.decodeBase64
