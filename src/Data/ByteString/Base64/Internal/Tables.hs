{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Data.ByteString.Base64.Internal.Tables
-- Copyright    : (c) 2019 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Shared internal tables
--
module Data.ByteString.Base64.Internal.Tables
( base64Table
, base64UrlTable
) where


import Data.ByteString.Base64.Internal.Utils

import Foreign.ForeignPtr

import GHC.Word

-- | Base64url encoding table
--
base64UrlTable :: EncodingTable
base64UrlTable = packTable "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"#
{-# NOINLINE base64UrlTable #-}

-- | Base64 std encoding table
--
base64Table :: EncodingTable
base64Table = packTable "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"#
{-# NOINLINE base64Table #-}
