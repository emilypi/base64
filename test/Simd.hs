{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Simd
-- Copyright    : (c) 2026 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- Public API regressions shared by the scalar and optional SIMD builds.
-- Expected encodings come from an independent implementation, including for
-- typed decoding: matching mistakes in our encoder and decoder cannot cancel.
module Simd (tests) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket, evaluate, try)
import Control.Monad (forM_, forever)
import "base64" Data.Base64.Types (Alphabet(StdPadded, UrlPadded, UrlUnpadded), assertBase64, extractBase64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import qualified "base64" Data.ByteString.Short.Base64 as SB64
import qualified "base64" Data.ByteString.Short.Base64.URL as SB64U
import qualified "base64" Data.ByteString.Base64 as B64
import qualified "base64" Data.ByteString.Base64.URL as B64U
import "base64" Data.ByteString.Base64.Internal.Head (decodeBase64_)
import "base64" Data.ByteString.Base64.Internal.Tables (decodeB64Table, decodeB64UrlTable)
import qualified "base64-bytestring" Data.ByteString.Base64 as Bos
import qualified "base64-bytestring" Data.ByteString.Base64.URL as BosU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Short as TS
import qualified "base64" Data.Text.Short.Encoding.Base64 as TS64
import qualified "base64" Data.Text.Short.Encoding.Base64.URL as TS64U
import Data.Text.Encoding.Error (UnicodeException)
import Data.Either (isLeft)
import Data.Word (Word8)
import GHC.Exts (isByteArrayPinned#, isTrue#)
import Internal (Harness(..), b64, lb64, sb64, t64, tl64, ts64)
import System.Mem (performGC)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)


tests :: TestTree
tests = testGroup "SIMD compatibility"
  [ testCase "vector boundaries and every slice alignment" $
      forM_ sizes $ \n ->
        forM_ [0 .. 63] $ \offset ->
          checkHarness (context n offset) b64 (sliceAt offset) (payload n)
  , testCase "every input byte across vector blocks" $
      forM_ [0 .. 255] $ \byte ->
        checkHarness ("repeated byte=" ++ show byte) b64
          (sliceAt (fromIntegral byte `mod` 64)) (BS.replicate 257 byte)
  , testCase "invalid characters retain absolute offsets" invalidCharacters
  , testCase "all high bytes are invalid in both alphabets" highBytes
  , testCase "padding and canonical tails after vector prefixes" paddingAndTails
  , testCase "padding errors precede invalid bytes in the same quartet" mixedErrors
  , testCase "lazy chunks split input and encoded quartets" lazyChunks
  , testCase "short bytestrings preserve large buffers and tails" $
      forM_ sizes $ \n ->
        checkHarness ("short length=" ++ show n) sb64 SBS.toShort (payload n)
  , testCase "short unpinned arrays remain valid across garbage collections" shortUnpinned
  , testCase "short decoders retain exact length and padding errors" shortErrors
  , testCase "short parsers retain bytes and text decoders reject invalid UTF-8" shortParsersAndUtf8
  , testCase "exposed checked decoder validates empty and incomplete inputs" checkedEntryPoint
  , testCase "Unicode text wrappers preserve long values and UTF-8" textWrappers
  ]
  where
    sizes = [0 .. 5]
      ++ concatMap (\n -> [n - 2 .. n + 2]) [12, 16, 24, 32, 48, 64, 96, 128, 192, 256]
      ++ [255, 256, 257, 4095, 4096, 4097, 65535, 65536, 65537]

-- The prefix and suffix are deliberately non-base64 bytes. Only the slice is
-- valid input, and its address varies independently of its logical contents.
sliceAt :: Int -> BS.ByteString -> BS.ByteString
sliceAt offset bytes = BS.take (BS.length bytes) . BS.drop offset $
  BS.replicate offset 0xe5 <> bytes <> BS.replicate 32 0xff

payload :: Int -> BS.ByteString
payload n = BS.pack (take n (cycle [0 .. 255]))

context :: Int -> Int -> String
context n offset = "length=" ++ show n ++ ", slice offset=" ++ show offset

checkHarness
  :: (Eq a, Show a)
  => String
  -> Harness a
  -> (BS.ByteString -> a)
  -> BS.ByteString
  -> Assertion
checkHarness note harness convert raw = do
  equal "standard encode" std (extractBase64 (encode harness input))
  equal "URL padded encode" url (extractBase64 (encodeUrl harness input))
  equal "URL unpadded encode" unpadded (extractBase64 (encodeUrlNopad harness input))
  equal "standard typed decode" input (decodeTyped harness (assertBase64 std))
  equal "standard untyped decode" (Right input) (decode harness std)
  equal "URL padded typed decode" input (decodeUrlTypedPad harness (assertBase64 url))
  equal "URL padded untyped decode" (Right input) (decodeUrlPad harness url)
  equal "URL unpadded typed decode" input
    (decodeUrlTypedNopad harness (assertBase64 unpadded))
  equal "URL unpadded untyped decode" (Right input) (decodeUrlNopad harness unpadded)
  equal "URL generic padded decode" (Right input) (decodeUrl harness url)
  equal "URL generic unpadded decode" (Right input) (decodeUrl harness unpadded)
  equal "URL generic padded typed decode" input
    (decodeUrlTyped harness (assertBase64 @'UrlPadded url))
  equal "URL generic unpadded typed decode" input
    (decodeUrlTyped harness (assertBase64 @'UrlUnpadded unpadded))
  where
    input = convert raw
    std = convert (Bos.encode raw)
    url = convert (BosU.encode raw)
    unpadded = convert (BS.takeWhile (/= 61) (BosU.encode raw))
    equal name = assertEqual (note ++ ": " ++ name)

-- Exercise invalid bytes in every lane, across native block boundaries, and
-- well after a valid SIMD prefix. A valid suffix prevents tail-only coverage.
invalidCharacters :: Assertion
invalidCharacters =
  forM_ [0, 1, 15, 31, 32, 47, 63] $ \offset ->
    forM_ positions $ \position -> do
      forM_ [35, 128, 255] $ \byte ->
        forM_ decoders $ \(name, decoder) ->
          checkInvalid name decoder offset position byte
      forM_ [45, 95] $ \byte ->
        forM_ standardDecoders $ \(name, decoder) ->
          checkInvalid (name ++ " rejects URL alphabet") decoder offset position byte
      forM_ [43, 47] $ \byte ->
        forM_ urlDecoders $ \(name, decoder) ->
          checkInvalid name decoder offset position byte
  where
    positions = [0 .. 19] ++ [31 .. 33] ++ [47 .. 49] ++ [63 .. 65]
      ++ [95 .. 97] ++ [127 .. 129] ++ [191 .. 193] ++ [255 .. 257]
      ++ [511 .. 513] ++ [4095 .. 4097]

highBytes :: Assertion
highBytes =
  forM_ [128 .. 255] $ \byte ->
    forM_ [15, 16, 31, 32, 63, 64, 127, 255, 256, 4096] $ \position ->
      forM_ decoders $ \(name, decoder) ->
        checkInvalid name decoder (fromIntegral byte `mod` 64) position byte

type Decoder = BS.ByteString -> Either T.Text BS.ByteString

decoders :: [(String, Decoder)]
decoders = standardDecoders ++ urlDecoders

standardDecoders :: [(String, Decoder)]
standardDecoders =
  [ ("standard", B64.decodeBase64Untyped)
  , ("short standard", fromShortDecoder SB64.decodeBase64Untyped)
  ]

urlDecoders :: [(String, Decoder)]
urlDecoders = urlPaddedDecoders ++ urlUnpaddedDecoders ++ urlGenericDecoders

urlPaddedDecoders :: [(String, Decoder)]
urlPaddedDecoders =
  [ ("URL padded", B64U.decodeBase64PaddedUntyped)
  , ("short URL padded", fromShortDecoder SB64U.decodeBase64PaddedUntyped)
  ]

urlUnpaddedDecoders :: [(String, Decoder)]
urlUnpaddedDecoders =
  [ ("URL unpadded", B64U.decodeBase64UnpaddedUntyped)
  , ("short URL unpadded", fromShortDecoder SB64U.decodeBase64UnpaddedUntyped)
  ]

urlGenericDecoders :: [(String, Decoder)]
urlGenericDecoders =
  [ ("URL generic", B64U.decodeBase64Untyped)
  , ("short URL generic", fromShortDecoder SB64U.decodeBase64Untyped)
  ]

fromShortDecoder
  :: (SBS.ShortByteString -> Either T.Text SBS.ShortByteString) -> Decoder
fromShortDecoder decoder = fmap SBS.fromShort . decoder . SBS.toShort

checkInvalid :: String -> Decoder -> Int -> Int -> Word8 -> Assertion
checkInvalid name decoder offset position byte =
  assertEqual note
    (Left (atOffset "invalid base64 encoding near offset: " position))
    (decoder input)
  where
    input = sliceAt offset $
      BS.replicate position 65 <> BS.singleton byte
        <> BS.replicate (position `div` 4 * 4 + 260 - position - 1) 65
    note = name ++ ": " ++ context (BS.length input) offset
      ++ ", invalid byte=" ++ show byte ++ " at=" ++ show position

atOffset :: T.Text -> Int -> T.Text
atOffset prefix offset = prefix <> T.pack (show offset)

mixedErrors :: Assertion
mixedErrors =
  forM_ [0, 32, 64, 128, 256, 4096] $ \prefixLength ->
    forM_ [0, 1, 15, 31] $ \offset ->
      forM_ [("#=AA", 1), ("A#=A", 2), ("#AA=", 3), ("=#AA", 0)] $
        \(quartet, paddingPosition) ->
          forM_ decoders $ \(name, decoder) ->
            assertEqual (name ++ ": " ++ context prefixLength offset
              ++ ", mixed errors=" ++ show quartet)
              (Left (atOffset "invalid padding near offset: "
                (prefixLength + paddingPosition)))
              (decoder (sliceAt offset
                (BS.replicate prefixLength 65 <> quartet <> BS.replicate 256 65)))

paddingAndTails :: Assertion
paddingAndTails =
  forM_ [0, 16, 32, 48, 64, 96, 128, 192, 256, 4096] $ \prefixLength ->
    forM_ [0, 1, 15, 31] $ \offset -> do
      let prefix = BS.replicate prefixLength 65
          input suffix = sliceAt offset (prefix <> suffix)
          note name = name ++ ": " ++ context prefixLength offset
          expected suffix = Right (BS.replicate (prefixLength `div` 4 * 3) 0 <> suffix)
      forM_ decoders $ \(name, decoder) ->
        forM_ [0 .. 3] $ \position ->
          assertEqual (note name ++ ", interior padding at=" ++ show position)
            (Left (atOffset "invalid padding near offset: " (prefixLength + position)))
            (decoder (input (BS.replicate position 65 <> "="
              <> BS.replicate (7 - position) 65)))
      forM_ (standardDecoders ++ urlPaddedDecoders ++ urlGenericDecoders) $ \(name, decoder) -> do
        assertEqual (note name ++ ", one-byte tail") (expected "d")
          (decoder (input "ZA=="))
        assertEqual (note name ++ ", two-byte tail") (expected "f`")
          (decoder (input "ZmA="))
        forM_ [("ZE==", 1), ("ZmC=", 2)] $ \(suffix, position) ->
          assertEqual (note name ++ ", noncanonical tail=" ++ show suffix)
            (Left (atOffset "non-canonical encoding detected at offset: "
              (prefixLength + position))) (decoder (input suffix))
      forM_ (urlUnpaddedDecoders ++ urlGenericDecoders) $ \(name, decoder) -> do
        assertEqual (note name ++ ", unpadded one-byte tail") (expected "d")
          (decoder (input "ZA"))
        assertEqual (note name ++ ", unpadded two-byte tail") (expected "f`")
          (decoder (input "ZmA"))
        forM_ [("ZE", 1), ("ZmC", 2)] $ \(suffix, position) ->
          assertEqual (note name ++ ", noncanonical unpadded tail=" ++ show suffix)
            (Left (atOffset "non-canonical encoding detected at offset: "
              (prefixLength + position))) (decoder (input suffix))
      assertEqual (note "unpadded rejects padding")
        (Left "Base64-encoded bytestring has invalid padding")
        (B64U.decodeBase64UnpaddedUntyped (input "ZA=="))
      assertEqual (note "padded requires padding")
        (Left "Base64-encoded bytestring requires padding")
        (B64U.decodeBase64PaddedUntyped (input "ZA"))
      forM_ decoders $ \(name, decoder) ->
        assertEqual (note name ++ ", invalid length")
          (Left "Base64-encoded bytestring has invalid size") (decoder (input "A"))

-- Force truly movable arrays on both sides of the native call, while another
-- capability requests collections. Every iteration has distinct contents.
shortUnpinned :: Assertion
shortUnpinned = bracket (forkIO (forever (performGC >> threadDelay 1000))) killThread $ \_ ->
  forM_ [0 .. 63 :: Int] $ \salt -> do
    let raw = BS.pack [fromIntegral (i + salt) | i <- [0 .. 256 + salt `mod` 3]]
        input = SBS.toShort raw
        encoded = extractBase64 (SB64.encodeBase64' input)
    assertUnpinned "source" input
    assertUnpinned "encoded destination" encoded
    assertEqual "direct encoding under GC" (SBS.toShort (Bos.encode raw)) encoded
    performGC
    checkHarness ("unpinned salt=" ++ show salt) sb64 SBS.toShort raw
    assertUnpinned "decoded destination" (SB64.decodeBase64 (assertBase64 @'StdPadded encoded))
  where
    assertUnpinned note (SBSI.SBS bytes#) =
      assertBool note (not (isTrue# (isByteArrayPinned# bytes#)))

-- Compare every checked short flavor with the established strict contract,
-- including incomplete quartets whose padding must remain virtual.
shortErrors :: Assertion
shortErrors =
  forM_ [0, 64, 128, 256, 4096] $ \prefixLength ->
    forM_ ["A", "AA", "AAA", "=", "A=", "AA=", "A==", "====", "AA=A", "#=AA",
           "ZA==", "ZE==", "ZmA=", "ZmC=", "ZE", "ZmC", "####", "\xff\x80\&AA"] $ \suffix ->
      forM_ pairs $ \(name, strict, short) -> do
        let input = BS.replicate prefixLength 65 <> suffix
        assertEqual (name ++ ": prefix=" ++ show prefixLength ++ ", suffix=" ++ show suffix)
          (strict input) (fromShortDecoder short input)
  where
    pairs =
      [ ("standard", B64.decodeBase64Untyped, SB64.decodeBase64Untyped)
      , ("URL padded", B64U.decodeBase64PaddedUntyped, SB64U.decodeBase64PaddedUntyped)
      , ("URL unpadded", B64U.decodeBase64UnpaddedUntyped, SB64U.decodeBase64UnpaddedUntyped)
      , ("URL generic", B64U.decodeBase64Untyped, SB64U.decodeBase64Untyped)
      ]

shortParsersAndUtf8 :: Assertion
shortParsersAndUtf8 = do
  forM_ ["ZA==", "ZE==", "ZmA=", "ZmC=", "A", "####"] $ \suffix -> do
    let bytes = BS.replicate 256 65 <> suffix
        short = SBS.toShort bytes
        text = TS.fromText (T.decodeUtf8 bytes)
        expected = bytes <$ B64.decodeBase64Untyped bytes
    assertEqual "short parser exact errors and retained input"
      expected (SBS.fromShort . extractBase64 <$> SB64.parseBase64 short)
    assertEqual "short text parser exact errors and retained input"
      expected (TS.toByteString . extractBase64 <$> TS64.parseBase64 text)
  let std = TS.fromText (T.replicate 64 "AAAA" <> "/w==")
      url = TS.fromText (T.replicate 64 "AAAA" <> "_w==")
      unpadded = TS.fromText (T.replicate 64 "AAAA" <> "_w")
  assertEqual "parser does not decode invalid UTF-8 payloads" (Right std)
    (extractBase64 <$> TS64.parseBase64 std)
  forM_
    [ TS64.decodeBase64 (assertBase64 @'StdPadded std)
    , either (error . T.unpack) id (TS64.decodeBase64Untyped std)
    , TS64U.decodeBase64Padded (assertBase64 url)
    , either (error . T.unpack) id (TS64U.decodeBase64PaddedUntyped url)
    , TS64U.decodeBase64Unpadded (assertBase64 unpadded)
    , either (error . T.unpack) id (TS64U.decodeBase64UnpaddedUntyped unpadded)
    , either (error . T.unpack) id (TS64U.decodeBase64Untyped url)
    ] $ \decoded -> do
      result <- try (evaluate (TS.length decoded)) :: IO (Either UnicodeException Int)
      assertBool "invalid UTF-8 still raises UnicodeException" (isLeft result)

checkedEntryPoint :: Assertion
checkedEntryPoint =
  forM_ [decodeB64Table, decodeB64UrlTable] $ \table ->
    forM_ [0, 1, 2, 3, 5, 6, 7, 125, 126, 127, 129, 130, 131] $ \size -> do
      let bytes = BS.replicate size 65
      actual <- decodeBase64_ table bytes
      assertEqual ("checked entry point length=" ++ show size)
        (B64.decodeBase64Untyped bytes) actual

lazyChunks :: Assertion
lazyChunks =
  forM_ [47, 48, 49, 95, 96, 97, 127, 128, 129, 4096] $ \n ->
    forM_ [1, 2, 3, 4, 11, 12, 15, 16, 47, 48, 63, 64, 127] $ \chunkSize ->
      checkHarness ("lazy length=" ++ show n ++ ", chunk=" ++ show chunkSize)
        lb64 (LBS.fromChunks . chunksOf chunkSize) (payload n)

chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf n bytes
  | BS.null bytes = []
  | otherwise = let (chunk, rest) = BS.splitAt n bytes
                in sliceAt 1 chunk : chunksOf n rest

textWrappers :: Assertion
textWrappers =
  forM_ [0, 1, 15, 16, 31, 32, 127, 1024] $ \repetitions -> do
    let value = T.replicate repetitions "a\x00e9\x03bb\x1f642"
        raw = T.encodeUtf8 value
        note = "text repetitions=" ++ show repetitions
    checkHarness note t64 T.decodeUtf8 raw
    checkHarness note ts64 (TS.fromText . T.decodeUtf8) raw
    forM_ [1, 15, 16, 63, 64] $ \chunkSize ->
      checkHarness (note ++ ", text chunk=" ++ show chunkSize) tl64
        (TL.fromChunks . T.chunksOf chunkSize . T.decodeUtf8) raw
