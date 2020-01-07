# Revision history for base64

## 0.3.0.0 -- 2020-01-07

* After a discussion with lexilambda, we're making 'encodeBase64' be `ByteString -> Text` by default, offering `ByteString -> ByteString` as
  a secondary format.
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
