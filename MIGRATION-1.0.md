Migration Guide for 1.0
----

Between the last major version (0.4.2.4) and the current major epoch (1.0), many API-related constructs have changed, and I'd like to justify them here and now so that users may have an immortalized explanation for what is most likely a disruptive change to their code.

## A faster loop

First, I'd like to say that I don't *like* breaking people's code. As an author and maintainer, I try and make sure that any API breakages are justified either by a significant UX improvement, or by a measurable performance increase large enough to warrant such a breakage. As such, I believe both of these criteria are met by the 0.4.x -> 1.0 upgrade: not only is the API safer to use, but the use of type data to establish the provenance of values encoded by this library also allows the performance-sensitive loops to be much cleaner, eschewing error checking where type data suffices. To prove this point, I've benchmarked the library between these last two epochs. The benchmarks say it all (all benchmarks are done on a Thinkpad P15 Gen 2 Intel i9-11950H, 64GB DDR4, Ubuntu 22.04 with GHC 9.6.3 stock, -O2):

In `base64-0.4.2.4`:

```
benchmarking encode/25/base64-bytestring
time                 49.97 ns   (49.87 ns .. 50.07 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 49.96 ns   (49.86 ns .. 50.14 ns)
std dev              440.1 ps   (235.9 ps .. 806.9 ps)

benchmarking encode/25/base64
time                 34.07 ns   (33.62 ns .. 34.56 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 33.88 ns   (33.77 ns .. 34.08 ns)
std dev              504.2 ps   (268.7 ps .. 773.8 ps)
variance introduced by outliers: 18% (moderately inflated)

benchmarking encode/100/base64-bytestring
time                 111.4 ns   (110.6 ns .. 112.4 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 111.7 ns   (111.3 ns .. 112.3 ns)
std dev              1.787 ns   (1.421 ns .. 2.247 ns)
variance introduced by outliers: 19% (moderately inflated)

benchmarking encode/100/base64
time                 53.39 ns   (53.19 ns .. 53.72 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 54.17 ns   (53.60 ns .. 56.40 ns)
std dev              3.163 ns   (1.151 ns .. 6.269 ns)
variance introduced by outliers: 78% (severely inflated)

benchmarking encode/1k/base64-bytestring
time                 754.3 ns   (750.8 ns .. 759.1 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 766.1 ns   (761.4 ns .. 771.5 ns)
std dev              17.44 ns   (14.17 ns .. 21.34 ns)
variance introduced by outliers: 29% (moderately inflated)

benchmarking encode/1k/base64
time                 274.6 ns   (273.2 ns .. 275.9 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 276.5 ns   (275.4 ns .. 277.6 ns)
std dev              3.863 ns   (3.413 ns .. 4.464 ns)
variance introduced by outliers: 14% (moderately inflated)

benchmarking encode/10k/base64-bytestring
time                 7.069 μs   (7.054 μs .. 7.094 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.100 μs   (7.088 μs .. 7.114 μs)
std dev              44.37 ns   (37.56 ns .. 54.14 ns)

benchmarking encode/10k/base64
time                 2.384 μs   (2.364 μs .. 2.415 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 2.370 μs   (2.363 μs .. 2.395 μs)
std dev              42.25 ns   (12.58 ns .. 86.70 ns)
variance introduced by outliers: 18% (moderately inflated)

benchmarking encode/100k/base64-bytestring
time                 70.59 μs   (70.26 μs .. 70.84 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 70.11 μs   (69.95 μs .. 70.28 μs)
std dev              587.0 ns   (508.0 ns .. 684.0 ns)

benchmarking encode/100k/base64
time                 23.31 μs   (23.22 μs .. 23.42 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 23.59 μs   (23.49 μs .. 23.72 μs)
std dev              415.2 ns   (343.8 ns .. 509.2 ns)
variance introduced by outliers: 14% (moderately inflated)

benchmarking encode/1mm/base64-bytestring
time                 703.6 μs   (700.6 μs .. 708.7 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 703.1 μs   (699.8 μs .. 720.0 μs)
std dev              18.82 μs   (5.505 μs .. 43.88 μs)
variance introduced by outliers: 17% (moderately inflated)

benchmarking encode/1mm/base64
time                 238.4 μs   (235.5 μs .. 241.4 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 234.6 μs   (233.4 μs .. 236.5 μs)
std dev              4.771 μs   (3.256 μs .. 7.810 μs)
variance introduced by outliers: 13% (moderately inflated)

benchmarking decode/25/base64-bytestring
time                 54.36 ns   (54.18 ns .. 54.55 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 55.11 ns   (54.74 ns .. 55.63 ns)
std dev              1.441 ns   (1.090 ns .. 2.068 ns)
variance introduced by outliers: 41% (moderately inflated)

benchmarking decode/25/base64
time                 53.04 ns   (52.57 ns .. 53.80 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 53.42 ns   (53.07 ns .. 53.93 ns)
std dev              1.378 ns   (1.061 ns .. 1.774 ns)
variance introduced by outliers: 40% (moderately inflated)

benchmarking decode/100/base64-bytestring
time                 145.2 ns   (143.8 ns .. 146.9 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 145.3 ns   (144.6 ns .. 146.5 ns)
std dev              3.165 ns   (2.441 ns .. 4.254 ns)
variance introduced by outliers: 30% (moderately inflated)

benchmarking decode/100/base64
time                 140.6 ns   (140.0 ns .. 141.2 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 140.6 ns   (140.2 ns .. 141.4 ns)
std dev              1.984 ns   (1.243 ns .. 3.410 ns)
variance introduced by outliers: 16% (moderately inflated)

benchmarking decode/1k/base64-bytestring
time                 1.115 μs   (1.112 μs .. 1.118 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.120 μs   (1.118 μs .. 1.123 μs)
std dev              8.290 ns   (6.907 ns .. 10.42 ns)

benchmarking decode/1k/base64
time                 1.109 μs   (1.102 μs .. 1.119 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.104 μs   (1.102 μs .. 1.108 μs)
std dev              9.031 ns   (4.358 ns .. 17.14 ns)

benchmarking decode/10k/base64-bytestring
time                 10.86 μs   (10.84 μs .. 10.89 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.90 μs   (10.88 μs .. 10.93 μs)
std dev              93.78 ns   (71.73 ns .. 143.6 ns)

benchmarking decode/10k/base64
time                 10.68 μs   (10.65 μs .. 10.72 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.68 μs   (10.66 μs .. 10.70 μs)
std dev              51.31 ns   (36.41 ns .. 70.46 ns)

benchmarking decode/100k/base64-bytestring
time                 108.4 μs   (108.0 μs .. 108.8 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 108.1 μs   (108.0 μs .. 108.4 μs)
std dev              643.5 ns   (450.9 ns .. 1.043 μs)

benchmarking decode/100k/base64
time                 106.0 μs   (105.9 μs .. 106.2 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 106.1 μs   (106.0 μs .. 106.3 μs)
std dev              586.1 ns   (405.8 ns .. 932.3 ns)

benchmarking decode/1mm/base64-bytestring
time                 1.076 ms   (1.074 ms .. 1.079 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.080 ms   (1.078 ms .. 1.082 ms)
std dev              6.833 μs   (5.938 μs .. 7.717 μs)

benchmarking decode/1mm/base64
time                 1.054 ms   (1.050 ms .. 1.056 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.051 ms   (1.049 ms .. 1.052 ms)
std dev              4.359 μs   (3.498 μs .. 5.253 μs)

```

vs in `base64-1.0.0.0`:

```
benchmarking encode/25/base64-bytestring
time                 52.04 ns   (51.77 ns .. 52.43 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 52.23 ns   (52.02 ns .. 52.50 ns)
std dev              790.3 ps   (649.7 ps .. 981.1 ps)
variance introduced by outliers: 19% (moderately inflated)

benchmarking encode/25/base64
time                 35.88 ns   (35.50 ns .. 36.15 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 35.44 ns   (35.28 ns .. 35.61 ns)
std dev              609.5 ps   (466.8 ps .. 835.9 ps)
variance introduced by outliers: 23% (moderately inflated)

benchmarking encode/100/base64-bytestring
time                 116.5 ns   (115.6 ns .. 117.5 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 119.1 ns   (117.9 ns .. 120.9 ns)
std dev              4.946 ns   (3.674 ns .. 6.871 ns)
variance introduced by outliers: 62% (severely inflated)

benchmarking encode/100/base64
time                 54.59 ns   (54.15 ns .. 54.97 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 54.84 ns   (54.53 ns .. 55.11 ns)
std dev              967.4 ps   (759.0 ps .. 1.233 ns)
variance introduced by outliers: 24% (moderately inflated)

benchmarking encode/1k/base64-bytestring
time                 792.6 ns   (789.2 ns .. 796.2 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 797.4 ns   (794.2 ns .. 801.2 ns)
std dev              12.70 ns   (10.10 ns .. 16.66 ns)
variance introduced by outliers: 17% (moderately inflated)

benchmarking encode/1k/base64
time                 300.4 ns   (296.8 ns .. 304.4 ns)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 294.8 ns   (291.3 ns .. 301.3 ns)
std dev              14.55 ns   (9.522 ns .. 25.93 ns)
variance introduced by outliers: 68% (severely inflated)

benchmarking encode/10k/base64-bytestring
time                 7.852 μs   (7.806 μs .. 7.917 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 7.849 μs   (7.810 μs .. 7.923 μs)
std dev              172.2 ns   (88.74 ns .. 277.8 ns)
variance introduced by outliers: 23% (moderately inflated)

benchmarking encode/10k/base64
time                 2.748 μs   (2.724 μs .. 2.773 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 2.737 μs   (2.717 μs .. 2.802 μs)
std dev              108.8 ns   (48.74 ns .. 219.4 ns)
variance introduced by outliers: 53% (severely inflated)

benchmarking encode/100k/base64-bytestring
time                 81.01 μs   (80.45 μs .. 81.98 μs)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 80.95 μs   (80.48 μs .. 82.34 μs)
std dev              2.561 μs   (1.019 μs .. 4.866 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking encode/100k/base64
time                 26.20 μs   (26.13 μs .. 26.29 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 26.32 μs   (26.25 μs .. 26.40 μs)
std dev              238.5 ns   (190.6 ns .. 314.7 ns)

benchmarking encode/1mm/base64-bytestring
time                 793.2 μs   (791.7 μs .. 794.8 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 795.1 μs   (794.0 μs .. 796.2 μs)
std dev              3.646 μs   (3.048 μs .. 4.402 μs)

benchmarking encode/1mm/base64
time                 266.1 μs   (260.7 μs .. 273.8 μs)
                     0.997 R²   (0.995 R² .. 1.000 R²)
mean                 262.2 μs   (260.6 μs .. 265.8 μs)
std dev              7.496 μs   (1.432 μs .. 12.40 μs)
variance introduced by outliers: 23% (moderately inflated)

benchmarking decode/25/base64-bytestring
time                 59.26 ns   (59.18 ns .. 59.35 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 59.31 ns   (59.22 ns .. 59.41 ns)
std dev              329.7 ps   (270.9 ps .. 416.3 ps)

benchmarking decode/25/base64-typed
time                 45.90 ns   (45.78 ns .. 46.04 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 45.95 ns   (45.88 ns .. 46.04 ns)
std dev              261.9 ps   (218.3 ps .. 327.6 ps)

benchmarking decode/25/base64-untyped
time                 55.79 ns   (55.63 ns .. 56.02 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 55.90 ns   (55.77 ns .. 56.06 ns)
std dev              470.0 ps   (364.5 ps .. 692.0 ps)

benchmarking decode/100/base64-bytestring
time                 153.8 ns   (153.4 ns .. 154.2 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 153.6 ns   (153.4 ns .. 153.9 ns)
std dev              931.2 ps   (780.6 ps .. 1.139 ns)

benchmarking decode/100/base64-typed
time                 121.3 ns   (120.6 ns .. 122.4 ns)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 121.5 ns   (120.6 ns .. 125.2 ns)
std dev              4.717 ns   (1.474 ns .. 10.23 ns)
variance introduced by outliers: 59% (severely inflated)

benchmarking decode/100/base64-untyped
time                 153.2 ns   (152.9 ns .. 153.5 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 153.3 ns   (153.1 ns .. 153.5 ns)
std dev              642.7 ps   (538.6 ps .. 804.8 ps)

benchmarking decode/1k/base64-bytestring
time                 1.246 μs   (1.244 μs .. 1.248 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.247 μs   (1.245 μs .. 1.248 μs)
std dev              4.807 ns   (3.911 ns .. 5.909 ns)

benchmarking decode/1k/base64-typed
time                 909.0 ns   (902.6 ns .. 919.9 ns)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 905.9 ns   (902.1 ns .. 917.4 ns)
std dev              19.73 ns   (7.516 ns .. 39.01 ns)
variance introduced by outliers: 27% (moderately inflated)

benchmarking decode/1k/base64-untyped
time                 1.210 μs   (1.192 μs .. 1.226 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.222 μs   (1.214 μs .. 1.226 μs)
std dev              19.44 ns   (10.77 ns .. 29.88 ns)
variance introduced by outliers: 16% (moderately inflated)

benchmarking decode/10k/base64-bytestring
time                 11.56 μs   (11.53 μs .. 11.59 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.49 μs   (11.47 μs .. 11.52 μs)
std dev              91.19 ns   (77.80 ns .. 110.9 ns)

benchmarking decode/10k/base64-typed
time                 8.140 μs   (8.126 μs .. 8.157 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.141 μs   (8.125 μs .. 8.169 μs)
std dev              70.34 ns   (47.20 ns .. 119.2 ns)

benchmarking decode/10k/base64-untyped
time                 11.25 μs   (11.24 μs .. 11.27 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 11.29 μs   (11.27 μs .. 11.35 μs)
std dev              102.4 ns   (52.23 ns .. 185.3 ns)

benchmarking decode/100k/base64-bytestring
time                 114.2 μs   (113.9 μs .. 114.6 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 114.5 μs   (114.3 μs .. 114.8 μs)
std dev              778.0 ns   (644.2 ns .. 997.4 ns)

benchmarking decode/100k/base64-typed
time                 80.52 μs   (80.37 μs .. 80.68 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 80.56 μs   (80.44 μs .. 80.75 μs)
std dev              478.9 ns   (347.2 ns .. 750.1 ns)

benchmarking decode/100k/base64-untyped
time                 111.0 μs   (110.8 μs .. 111.2 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 111.4 μs   (111.2 μs .. 111.8 μs)
std dev              836.7 ns   (409.8 ns .. 1.471 μs)

benchmarking decode/1mm/base64-bytestring
time                 1.125 ms   (1.122 ms .. 1.128 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.123 ms   (1.121 ms .. 1.124 ms)
std dev              5.065 μs   (4.293 μs .. 6.589 μs)

benchmarking decode/1mm/base64-typed
time                 804.8 μs   (802.3 μs .. 807.7 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 802.7 μs   (802.0 μs .. 803.8 μs)
std dev              2.940 μs   (1.813 μs .. 4.985 μs)

benchmarking decode/1mm/base64-untyped
time                 1.108 ms   (1.106 ms .. 1.110 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.108 ms   (1.107 ms .. 1.110 ms)
std dev              5.673 μs   (4.383 μs .. 7.451 μs)

```

Benchmarks are included in this repo for you to reproduce these results on your own. You can see a parity in the `encode` step between the previous library iterations and the new epoch, with a *marked* improvement in decode speed (up to 25% faster on average between the old and new versions in the optimal case, and up to 40% in the suboptimal case) which justifies the performance aspect to me. Without deferring to pipelining instructions, hex encoding can only get so fast. In the future, this change also opens the library up to an optimal SIMD implementations.

## A sounder api

Second, I do not believe that these changes are unsound or overburdensome to the point that a migration to the new paradigm would be untenable. While it may be inconvenient to unwrap `Base64` types, in the `encode` case (all one must do is call `extractBase64` to extract the value from its wrapper, all caveats implied), and in the case of `decode`, an untyped variant is supplied, and is semantically consistent with the old behavior (the loop is the same). Hence, a migration is fairly easy to sketch out:

```
"encodeBase64'" -> "extractBase64 . encodeBase64'"
"encodeBase64" -> "extractBase64 . encodeBase64"
"decodebase64" -> "decodeBase64Untyped"
"decodeBase64Unpadded" -> "decodeBase64UnpaddedUntyped"
"decodeBase64Padded" -> "decodeBase64PaddedUntyped"
"decodeBase64W*With" -> "decodeBase64*WithUntyped"
```

And that is all. In order to make use of the new loops, one must only use one of the blessed encode functions to generate a wrapped `Base64` value, or call `assertBase64` and proceed with using `decodeBase64` as usual in order to decode. You'll note that an untyped `encodeBase64` is not supplied, and this is due to the fact that it's trivial to extract a `Base64` encoded value once you have it. However, I want to encourage people to use the new API, so I have only supplied a decode with error checking in the untyped case, because sometimes we deal with other people's data and cannot establish provenance. In the encode case, I would rather keep that provenance a part of the API, and the user may opt to strip that data upon sending to others or around their systems. It's not my problem at that point!
