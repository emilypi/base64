-- |
-- Module       : Data.ByteString.Base64.URL
-- Copyright 	: (c) 2019 Emily Pillmore
-- License	: BSD-style
--
-- Maintainer	: Emily Pillmore <emilypi@cohomolo.gy>
-- Stability	: Experimental
-- Portability	: portable
--
-- This module contains the combinators implementing the
-- RFC 4648 specification for the Base64-URL encoding including
-- unpadded and lenient variants
--
module Data.ByteString.Base64.BSD
( encodeBase64
, decodeBase64
, decodeBase64Lenient
, encodeBase64Unpadded
, decodeBase64Unpadded
, decodeBase64UnpaddedLenient
) where

import Data.ByteString (ByteString)
import Data.Text (Text)


encodeBase64 :: ByteString -> ByteString
encodeBase64 = undefined

decodeBase64 :: ByteString -> Either Text ByteString
decodeBase64 = undefined

decodeBase64Lenient :: ByteString -> ByteString
decodeBase64Lenient = undefined

encodeBase64Unpadded :: ByteString -> ByteString
encodeBase64Unpadded = undefined

decodeBase64Unpadded :: ByteString -> Either Text ByteString
decodeBase64Unpadded = undefined

decodeBase64UnpaddedLenient :: ByteString -> ByteString
decodeBase64UnpaddedLenient = undefined
