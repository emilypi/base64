{-# LANGUAGE RankNTypes #-}

module Data.Base64(Alphabet(..), Base64, assertBase64, extractBase64) where

import Data.Base64.Internal(Alphabet(..), Base64(..))

-- | Assert a value to be encoded in a specific way
--
assertBase64 :: forall k a. a -> Base64 k a
assertBase64 = Base64

-- | Forget that a particular value is Base64-encoded
--
extractBase64 :: Base64 k a -> a
extractBase64 (Base64 a) = a