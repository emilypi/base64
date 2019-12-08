-- |
-- Module       : Data.Text.Encoding.Base64
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base64 encoding including
-- unpadded and lenient variants
--
module Data.Text.Encoding.Base64
( encodeBase64
, decodeBase64
, encodeBase64Unpadded
, decodeBase64Unpadded
) where


import qualified Data.ByteString.Base64 as B64

import Data.Text (Text)
import qualified Data.Text.Encoding as T


encodeBase64 :: Text -> Text
encodeBase64 = T.decodeUtf8 . B64.encodeBase64 . T.encodeUtf8

decodeBase64 :: Text -> Either Text Text
decodeBase64 = fmap T.decodeUtf8 . B64.decodeBase64 . T.encodeUtf8

encodeBase64Unpadded :: Text -> Text
encodeBase64Unpadded = T.decodeUtf8
    . B64.encodeBase64Unpadded
    . T.encodeUtf8

decodeBase64Unpadded :: Text -> Either Text Text
decodeBase64Unpadded = fmap T.decodeUtf8
    . B64.decodeBase64Unpadded
    . T.encodeUtf8
