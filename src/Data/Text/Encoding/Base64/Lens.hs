{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Text.Encoding.Base64.Lens
( -- * Classy Optics (re-exported)
  HasBase64(..)
, base64Of
)where


import Control.Lens

import Data.Text
import Data.ByteString.Base64.Lens
import qualified Data.Text.Encoding.Base64 as B64
import qualified Data.Text.Encoding.Base64.URL as B64U


instance HasBase64 Text where
    _Base64 = prism' B64.encodeBase64Text $ \b ->
      case B64.decodeBase64Text b of
        Right bs -> Just bs
        _ -> Nothing


    _Base64Url = prism' B64U.encodeBase64Text $ \b ->
      case B64U.decodeBase64Text b of
        Right bs -> Just bs
        _ -> Nothing

    _Base64Lenient = iso
      B64.encodeBase64Text
      B64.decodeBase64TextLenient

    _Base64UrlLenient = iso
      B64U.encodeBase64Text
      B64U.decodeBase64TextLenient

    _Base64Unpadded = prism' B64.encodeBase64Text $ \b ->
      case B64.decodeBase64TextUnpadded b of
        Right bs -> Just bs
        _ -> Nothing

    _Base64UrlUnpadded = prism' B64U.encodeBase64Text $ \b ->
      case B64U.decodeBase64TextUnpadded b of
        Right bs -> Just bs
        _ -> Nothing

    _Base64LenientUnpadded = iso
      B64.encodeBase64Text
      B64.decodeBase64TextLenient

    _Base64UrlLenientUnpadded = iso
      B64U.encodeBase64Text
      B64U.decodeBase64TextLenient
