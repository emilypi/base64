## Performance

The story so far:

- Good encoding/decoding performance for bytestrings ∊ \[0, 100\[ compared to `base64-bytestring`
- Good encoding/decoding performance for bytestrings ∊ ]100, 1000\[ compared to `base64-bytestring`
- Good improvement in encoding/decoding performance for bytestrings ∊ ]1000, 10,000[ compared to `base64-bytestring`
- Good improvement in encoding/decoding performance for bytestrings ∊ ]10,000, 1,000,000] compared to `base64-bytestring`
- Smaller heap footprint in general.

In general, we're seeing this on a MBP, i7 8 core, 16GB ram, GHC 8.6.5 (sizes are in bytes):

```
benchmarked encode/base64-bytestring/25
time                 40.61 ns   (40.20 ns .. 41.10 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 40.79 ns   (40.50 ns .. 41.11 ns)
std dev              1.056 ns   (883.5 ps .. 1.273 ns)
variance introduced by outliers: 11% (moderately inflated)

benchmarked encode/base64-bytestring/100
time                 80.58 ns   (79.45 ns .. 81.44 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 80.22 ns   (79.60 ns .. 81.38 ns)
std dev              2.858 ns   (1.669 ns .. 4.625 ns)
variance introduced by outliers: 18% (moderately inflated)

benchmarked encode/base64-bytestring/1000
time                 559.5 ns   (555.6 ns .. 565.0 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 568.5 ns   (565.6 ns .. 571.8 ns)
std dev              10.98 ns   (8.950 ns .. 13.80 ns)

benchmarked encode/base64-bytestring/10000
time                 5.151 μs   (5.111 μs .. 5.199 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 5.192 μs   (5.165 μs .. 5.226 μs)
std dev              107.3 ns   (78.42 ns .. 153.1 ns)

benchmarked encode/base64-bytestring/100000
time                 51.45 μs   (50.89 μs .. 51.95 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 51.57 μs   (51.26 μs .. 52.04 μs)
std dev              1.354 μs   (1.015 μs .. 2.366 μs)
variance introduced by outliers: 10% (moderately inflated)

benchmarked encode/base64-bytestring/1000000
time                 515.8 μs   (510.4 μs .. 521.0 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 512.4 μs   (508.8 μs .. 517.2 μs)
std dev              13.82 μs   (11.00 μs .. 19.44 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarked encode/base64/25
time                 36.53 ns   (36.28 ns .. 36.85 ns)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 36.42 ns   (36.21 ns .. 36.70 ns)
std dev              760.4 ps   (592.1 ps .. 1.092 ns)

benchmarked encode/base64/100
time                 65.59 ns   (64.93 ns .. 66.19 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 65.39 ns   (65.07 ns .. 65.92 ns)
std dev              1.313 ns   (939.8 ps .. 2.147 ns)

benchmarked encode/base64/1000
time                 432.9 ns   (424.7 ns .. 439.8 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 426.8 ns   (424.7 ns .. 429.6 ns)
std dev              8.503 ns   (6.660 ns .. 10.69 ns)

benchmarked encode/base64/10000
time                 3.894 μs   (3.821 μs .. 3.952 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 3.921 μs   (3.891 μs .. 3.959 μs)
std dev              114.6 ns   (90.22 ns .. 160.9 ns)
variance introduced by outliers: 13% (moderately inflated)

benchmarked encode/base64/100000
time                 37.99 μs   (37.62 μs .. 38.41 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 38.23 μs   (38.00 μs .. 38.58 μs)
std dev              946.7 ns   (679.6 ns .. 1.416 μs)

benchmarked encode/base64/1000000
time                 369.5 μs   (366.6 μs .. 373.2 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 377.7 μs   (375.3 μs .. 380.2 μs)
std dev              8.312 μs   (6.889 μs .. 10.10 μs)

```

### Implementation Notes

In general, I have been following Alfred Klomp's [base64](https://github.com/aklomp/base64) example, having previously exhausted the algorithms used by `memory` and `base64-bytestring` in terms of performance. Base64 encodings have been factored into a 3-part series of phases:

- A loop _head_, which organizes the data and initial state
- An _inner loop_, which does the meat of the encoding recursively
- A loop _tail_, which encodes the final quantum of the encoding, adding any padding if requested. 

The inner loops are where optimizations are most visibly seen. In the case of this library, different loops are chosen dependent on the system architecture: 32-bit and 64-bit machines have different native machine word sizes, and so, have different cache lines that we can exploit for performance. In general, we want to try and fit as many encoded words into each iteration of the encoding loop as possible. 64-bit machines, for instance, cram 48 bits into every word, where 32-bit machines cram 24 bits into every 32-bit word, which means 64-bit machines are doing 2x _fewer_ loops on average, and twice the work. This does not translate holistically into 2x performance, because recursion is so well-optimized, but it does result in a noticable speedup. All other architectures defer to the standard `Word16` loop, though, I think this can be amended to be "anything with a word size >= 64 should use the 64-bit version" etc. 

Decoding follows a similar vein. 

### To come later

Daniel Lemire has a great [blog post](https://lemire.me/blog/2018/01/17/ridiculously-fast-base64-encoding-and-decoding/) detailing how he and Wojciech Mula implemented fast AVX and SSE base64 vectorization with a nifty library to boot. This has all been incorporated into Alfred Klomp's [base64](https://github.com/aklomp/base64) library, which is very up to date. The speed up people see is *massive*, and is BSD-2. We can inline the useful portions of the library along with the copyright notice in `cbits` and be very happy. Vectorization would be a huge win for the community. 

Future performance is tracked here in [this issue](https://github.com/emilypi/base64/issues/7)

