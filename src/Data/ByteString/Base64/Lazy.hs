-- |
-- Module       : Data.ByteString.Base64
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the lazy combinators implementing the
-- RFC 4648 specification for the Base64 encoding including
-- unpadded and lenient variants
--
module Data.ByteString.Base64.Lazy
( encodeBase64
, decodeBase64
, encodeBase64Unpadded
, decodeBase64Unpadded
) where


import Data.ByteString (ByteString)
import Data.Text (Text)

encodeBase64 :: ByteString -> ByteString
encodeBase64 = undefined

decodeBase64 :: ByteString -> Either Text ByteString
decodeBase64 = undefined

encodeBase64Unpadded :: ByteString -> ByteString
encodeBase64Unpadded = undefined

decodeBase64Unpadded :: ByteString -> Either Text ByteString
decodeBase64Unpadded = undefined
