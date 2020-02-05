## Performance

The story so far:

- Good encoding/decoding performance for bytestrings ∊ \[0, 100\[ compared to `base64-bytestring`
- Good encoding/decoding performance for bytestrings ∊ ]100, 1000\[ compared to `base64-bytestring`
- Good improvement in encoding/decoding performance for bytestrings ∊ ]1000, 10,000[ compared to `base64-bytestring`
- Good improvement in encoding/decoding performance for bytestrings ∊ ]10,000, 1,000,000] compared to `base64-bytestring`
- Smaller heap footprint in general.

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

