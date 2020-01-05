# Base64

[![Build Status](https://travis-ci.com/emilypi/base64.svg?branch=master)](https://travis-ci.com/emilypi/base64)

Padded and unpadded base64 and base64url encodings


### Performance

The story so far:

- 3x encoding/decoding performance for bytestrings ∊ \[0, 100\[
- 2x encoding/decoding performance for bytestrings ∊ ]100, 1000\[
- Better performance improvements of 1-3μs up to 1,000,000, diminishing returns after. 

Most of this performance increase for smaller bytestrings is due to optimization of the building of the encoding tables. Additionally, I've factored out several read/writes and unnecessary IO done in `bytestring-base64`. The inner loop is as optimized as it can be for `Word16`-based optimizations (3 8-Byte reads, 2 12-Byte writes), and the tail completion is optimized by elimating 4 unnecessary reads, and using one big `if-then-else` branch to eliminate 1 read and 1 write per case. Smaller improvements have been the elimination of unnecessary wrappers (i.e. not using `BS.pack` and simply `malloc`'ing and rolling my own pointers), as well as properly inlining auxiliary functions which failed to inline in Bos' version. Additionally, using unpacked `Addr#`'s for the alphabet means we don't have to use a `ForeignPtr` box or touch it unnecessarily. See the outputs in the benchmarks directory for more detail.. 

### Improvements

My suspicion is that we can get away with 1 `Word32` read, discard the upper 8 bytes, do some bitshifting magic, and then do a single `Word32` (with only the lower 24 Bytes filled) write to the output pointer in the inner loop, eliminating 2 reads and 1 write to make it single read/write. Additionally, if we want to impose size constraints, we should be able to fill the bottom 48 Bytes of a `Word64` and still be on the right side of the cache line, eliminating 2x loops per iteration. Additionally, factoring out the encoding tables to a static `CString` sitting in `cbits` would eliminate the need to construct it each time - there are only 2 alphabets, and we can maintain two static tables. We can do the same with the alphabets, but this is dubious since unboxed strlit `Addr#` is cheap and if we can get away with not calling to the FFI, that would be preferable. In the end, why not just do the whole thing in C and call it a day though? (Thoughts?)

### To come later

Daniel Lemire has a great [blog post](https://lemire.me/blog/2018/01/17/ridiculously-fast-base64-encoding-and-decoding/) detailing how he and Wojciech Mula implemented fast AVX and SSE base64 vectorization with a nifty library to boot. This has all been incorporated into Alfred Klomp's [base64](https://github.com/aklomp/base64) library, which is very up to date. The speed up people see is *massive*, and is BSD-2. We can inline the useful portions of the library along with the copyright notice in `cbits` and be very happy. 


