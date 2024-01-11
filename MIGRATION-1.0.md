Migration Guide for 1.0
----

Between the last major version (0.4.2.4) and the current major epoch (1.0), many API-related constructs have changed, and I'd like to justify them here and now so that users may have an immortalized explanation for what is most likely a disruptive change to their code.

## A faster loop

First, I'd like to say that I don't *like* breaking people's code. As an author and maintainer, I try and make sure that any API breakages are justified either by a significant UX improvement, or by a measurable performance increase large enough to warrant such a breakage. As such, I believe both of these criteria are met by the 0.4.x -> 1.0 upgrade: not only is the API safer to use, but the use of type data to establish the provenance of values encoded by this library also allows the performance-sensitive loops to be much cleaner, eschewing error checking where type data suffices. To prove this point, I've benchmarked the library between these last two epochs. The benchmarks say it all (all benchmarks are done on a Thinkpad P15 Gen 2 Intel i9-11950H, 64GB DDR4, Ubuntu 22.04 with GHC 8.10.7 stock, -O2):

In `base64-0.4.2.4`:

```

```

vs in `base64-1.0.0.0`:

```
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
