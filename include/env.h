#ifndef BASE64_ENV_H
#define BASE64_ENV_H

// This header file contains macro definitions that describe certain aspects of
// the compile-time environment. Compatibility and portability macros go here.

// Define machine endianness. This is for GCC:
#if (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__)
#  define BASE64_LITTLE_ENDIAN 1
#else
#  define BASE64_LITTLE_ENDIAN 0
#endif

// This is for Clang:
#ifdef __LITTLE_ENDIAN__
#  define BASE64_LITTLE_ENDIAN 1
#endif

#ifdef __BIG_ENDIAN__
#  define BASE64_LITTLE_ENDIAN 0
#endif

#endif	// BASE64_ENV_H