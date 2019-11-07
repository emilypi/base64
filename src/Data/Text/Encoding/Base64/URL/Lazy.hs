module Data.Text.Encoding.Base64.URL.Lazy
( encodeBase64Text
, decodeBase64Text
, decodeBase64TextLenient
, encodeBase64TextUnpadded
, decodeBase64TextUnpadded
, decodeBase64TextUnpaddedLenient
) where


import Data.ByteString.Base64.URL.Lazy

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


encodeBase64Text :: Text -> Text
encodeBase64Text = T.decodeUtf8 . encodeBase64 . T.encodeUtf8

decodeBase64Text :: Text -> Either Text Text
decodeBase64Text = fmap T.decodeUtf8 . decodeBase64 . T.encodeUtf8

decodeBase64TextLenient :: Text -> Text
decodeBase64TextLenient = T.decodeUtf8
    . decodeBase64Lenient
    . T.encodeUtf8

encodeBase64TextUnpadded :: Text -> Text
encodeBase64TextUnpadded = T.decodeUtf8
    . encodeBase64Unpadded
    . T.encodeUtf8

decodeBase64TextUnpadded :: Text -> Either Text Text
decodeBase64TextUnpadded = fmap T.decodeUtf8
    . decodeBase64Unpadded
    . T.encodeUtf8

decodeBase64TextUnpaddedLenient :: Text -> Text
decodeBase64TextUnpaddedLenient = T.decodeUtf8
    . decodeBase64UnpaddedLenient
    . T.encodeUtf8
