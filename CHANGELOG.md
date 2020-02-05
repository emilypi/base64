# Revision history for base64

## 0.4.1 -- 2020-02-04

* Optimize loops for 32-bit and 64-bit architectures
* Restructure project to be more amenable to swapping head/tail/loops

## 0.4.0 -- 2020-01-26

* With this major version release, we remove the redundant `encodeBase64Unpadded` and `decodeBase64Unpadded` functions from `Base64.hs`. This is for two reasons:
  1. There is no reason for them to exist, since all std base64 is expected to be padded (in contrast to base64url)
  2. it was literally redundant with `decodeBase64`.

* Use a specialized `Bool` type to give better visual cues regarding which functions add padding

## 0.3.1.1 -- 2020-01-15

* Make sure benchmark code builds

## 0.3.1.0 -- 2020-01-08

* Bug fix for `isBase64` and `isBase64Url` - wrong alphabet was used
* Added `isValidBase64` and `isValidBase64Url` for alphabet conformity. The `isBase64*` functions now tell if it's *correct* base64 now in the sense that it's decodable and valid.
* Dropped Cabal version to 2.0 for backcompat with Stack
* Better documentation

## 0.3.0.0 -- 2020-01-07

* After a discussion with lexilambda, we're making 'encodeBase64' be `ByteString -> Text` by default, offering `ByteString -> ByteString` as
  a secondary format.
* Add `decodeBase64Lenient` to the API for phadej
* Fix unpadded decoding bug where garbage was appended to the end of garbage inputs. A cleaner way to do this is to simply encode as Base64 with
  padding and then strip padding chars until I come up with a workflow specific to unpadded inputs (I used to have this, so I'll have to dig it up)
* Added `isBase64` and `isBase64Url` to the API
* Performance is stable

## 0.2.0.0 -- 2020-01-05

* After a discussion with phadej, we're doing away with the flags, and splitting the optics out into their own separate library
* Removed unnecessary inline pragmas

## 0.1.0.0 -- 2020-01-05

* Do away with the typeclasses, and just provide prisms + synonyms
* Continued performance improvements to decoding
* Corrected benchmarks

## 0.0.1.0 -- 2020-01-03

* First version. Released on an unsuspecting world.
* Preliminary release
