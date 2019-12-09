# Base64


Padded and unpadded base64 and base64url encodings


### Performance

The story so far:

![benchmarking improvements to `base64-bytestring`'s algorithm](https://i.imgur.com/oBDiFPR.png)

Performance is steadily improving as I exhaust existing algorithms for optimizations. So far, the optimal Haskell implementation seems to need to rely on a way of getting a single `Word32` copy and write as opposed to Bos' two `Word16` copy and writes. Cbits may be necessary.

See [the report](bench/base64.html) for the full breakdown at `-O2`
