/* auto-generated on 2021-05-27 22:44:11 +0100. Do not edit! */
/* begin file include/simdjson.h */
#ifndef SIMDJSON_H
#define SIMDJSON_H

/**
 * @mainpage
 *
 * Check the [README.md](https://github.com/simdjson/simdjson/blob/master/README.md#simdjson--parsing-gigabytes-of-json-per-second).
 *
 * Sample code. See https://github.com/simdjson/simdjson/blob/master/doc/basics.md for more examples.

    #include "simdjson.h"

    int main(void) {
      // load from `twitter.json` file:
      simdjson::dom::parser parser;
      simdjson::dom::element tweets = parser.load("twitter.json");
      std::cout << tweets["search_metadata"]["count"] << " results." << std::endl;

      // Parse and iterate through an array of objects
      auto abstract_json = R"( [
        {  "12345" : {"a":12.34, "b":56.78, "c": 9998877}   },
        {  "12545" : {"a":11.44, "b":12.78, "c": 11111111}  }
        ] )"_padded;

      for (simdjson::dom::object obj : parser.parse(abstract_json)) {
        for(const auto key_value : obj) {
          cout << "key: " << key_value.key << " : ";
          simdjson::dom::object innerobj = key_value.value;
          cout << "a: " << double(innerobj["a"]) << ", ";
          cout << "b: " << double(innerobj["b"]) << ", ";
          cout << "c: " << int64_t(innerobj["c"]) << endl;
        }
      }
    }
 */

/* begin file include/simdjson/builtin.h */
#ifndef SIMDJSON_BUILTIN_H
#define SIMDJSON_BUILTIN_H

/* begin file include/simdjson/implementations.h */
#ifndef SIMDJSON_IMPLEMENTATIONS_H
#define SIMDJSON_IMPLEMENTATIONS_H

/* begin file include/simdjson/implementation-base.h */
#ifndef SIMDJSON_IMPLEMENTATION_BASE_H
#define SIMDJSON_IMPLEMENTATION_BASE_H

/**
 * @file
 *
 * Includes common stuff needed for implementations.
 */

/* begin file include/simdjson/base.h */
#ifndef SIMDJSON_BASE_H
#define SIMDJSON_BASE_H

/* begin file include/simdjson/compiler_check.h */
#ifndef SIMDJSON_COMPILER_CHECK_H
#define SIMDJSON_COMPILER_CHECK_H

#ifndef __cplusplus
#error simdjson requires a C++ compiler
#endif

#ifndef SIMDJSON_CPLUSPLUS
#if defined(_MSVC_LANG) && !defined(__clang__)
#define SIMDJSON_CPLUSPLUS (_MSC_VER == 1900 ? 201103L : _MSVC_LANG)
#else
#define SIMDJSON_CPLUSPLUS __cplusplus
#endif
#endif

// C++ 17
#if !defined(SIMDJSON_CPLUSPLUS17) && (SIMDJSON_CPLUSPLUS >= 201703L)
#define SIMDJSON_CPLUSPLUS17 1
#endif

// C++ 14
#if !defined(SIMDJSON_CPLUSPLUS14) && (SIMDJSON_CPLUSPLUS >= 201402L)
#define SIMDJSON_CPLUSPLUS14 1
#endif

// C++ 11
#if !defined(SIMDJSON_CPLUSPLUS11) && (SIMDJSON_CPLUSPLUS >= 201103L)
#define SIMDJSON_CPLUSPLUS11 1
#endif

#ifndef SIMDJSON_CPLUSPLUS11
#error simdjson requires a compiler compliant with the C++11 standard
#endif

#endif // SIMDJSON_COMPILER_CHECK_H
/* end file include/simdjson/compiler_check.h */
/* begin file include/simdjson/common_defs.h */
#ifndef SIMDJSON_COMMON_DEFS_H
#define SIMDJSON_COMMON_DEFS_H

#include <cassert>
/* begin file include/simdjson/portability.h */
#ifndef SIMDJSON_PORTABILITY_H
#define SIMDJSON_PORTABILITY_H

#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cfloat>
#include <cassert>
#ifndef _WIN32
// strcasecmp, strncasecmp
#include <strings.h>
#endif

#ifdef _MSC_VER
#define SIMDJSON_VISUAL_STUDIO 1
/**
 * We want to differentiate carefully between
 * clang under visual studio and regular visual
 * studio.
 *
 * Under clang for Windows, we enable:
 *  * target pragmas so that part and only part of the
 *     code gets compiled for advanced instructions.
 *
 */
#ifdef __clang__
// clang under visual studio
#define SIMDJSON_CLANG_VISUAL_STUDIO 1
#else
// just regular visual studio (best guess)
#define SIMDJSON_REGULAR_VISUAL_STUDIO 1
#endif // __clang__
#endif // _MSC_VER

#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
// https://en.wikipedia.org/wiki/C_alternative_tokens
// This header should have no effect, except maybe
// under Visual Studio.
#include <iso646.h>
#endif

#if defined(__x86_64__) || defined(_M_AMD64)
#define SIMDJSON_IS_X86_64 1
#elif defined(__aarch64__) || defined(_M_ARM64)
#define SIMDJSON_IS_ARM64 1
#elif defined(__PPC64__) || defined(_M_PPC64)
#define SIMDJSON_IS_PPC64 1
#else
#define SIMDJSON_IS_32BITS 1

// We do not support 32-bit platforms, but it can be
// handy to identify them.
#if defined(_M_IX86) || defined(__i386__)
#define SIMDJSON_IS_X86_32BITS 1
#elif defined(__arm__) || defined(_M_ARM)
#define SIMDJSON_IS_ARM_32BITS 1
#elif defined(__PPC__) || defined(_M_PPC)
#define SIMDJSON_IS_PPC_32BITS 1
#endif

#endif // defined(__x86_64__) || defined(_M_AMD64)

#ifdef SIMDJSON_IS_32BITS
#ifndef SIMDJSON_NO_PORTABILITY_WARNING
#pragma message("The simdjson library is designed \
for 64-bit processors and it seems that you are not \
compiling for a known 64-bit platform. All fast kernels \
will be disabled and performance may be poor. Please \
use a 64-bit target such as x64, 64-bit ARM or 64-bit PPC.")
#endif // SIMDJSON_NO_PORTABILITY_WARNING
#endif // SIMDJSON_IS_32BITS

// this is almost standard?
#undef STRINGIFY_IMPLEMENTATION_
#undef STRINGIFY
#define STRINGIFY_IMPLEMENTATION_(a) #a
#define STRINGIFY(a) STRINGIFY_IMPLEMENTATION_(a)

// Our fast kernels require 64-bit systems.
//
// On 32-bit x86, we lack 64-bit popcnt, lzcnt, blsr instructions.
// Furthermore, the number of SIMD registers is reduced.
//
// On 32-bit ARM, we would have smaller registers.
//
// The simdjson users should still have the fallback kernel. It is
// slower, but it should run everywhere.

//
// Enable valid runtime implementations, and select SIMDJSON_BUILTIN_IMPLEMENTATION
//

// We are going to use runtime dispatch.
#ifdef SIMDJSON_IS_X86_64
#ifdef __clang__
// clang does not have GCC push pop
// warning: clang attribute push can't be used within a namespace in clang up
// til 8.0 so SIMDJSON_TARGET_REGION and SIMDJSON_UNTARGET_REGION must be *outside* of a
// namespace.
#define SIMDJSON_TARGET_REGION(T)                                                       \
  _Pragma(STRINGIFY(                                                           \
      clang attribute push(__attribute__((target(T))), apply_to = function)))
#define SIMDJSON_UNTARGET_REGION _Pragma("clang attribute pop")
#elif defined(__GNUC__)
// GCC is easier
#define SIMDJSON_TARGET_REGION(T)                                                       \
  _Pragma("GCC push_options") _Pragma(STRINGIFY(GCC target(T)))
#define SIMDJSON_UNTARGET_REGION _Pragma("GCC pop_options")
#endif // clang then gcc

#endif // x86

// Default target region macros don't do anything.
#ifndef SIMDJSON_TARGET_REGION
#define SIMDJSON_TARGET_REGION(T)
#define SIMDJSON_UNTARGET_REGION
#endif

// Is threading enabled?
#if defined(_REENTRANT) || defined(_MT)
#ifndef SIMDJSON_THREADS_ENABLED
#define SIMDJSON_THREADS_ENABLED
#endif
#endif

// workaround for large stack sizes under -O0.
// https://github.com/simdjson/simdjson/issues/691
#ifdef __APPLE__
#ifndef __OPTIMIZE__
// Apple systems have small stack sizes in secondary threads.
// Lack of compiler optimization may generate high stack usage.
// Users may want to disable threads for safety, but only when
// in debug mode which we detect by the fact that the __OPTIMIZE__
// macro is not defined.
#undef SIMDJSON_THREADS_ENABLED
#endif
#endif


#if defined(__clang__)
#define NO_SANITIZE_UNDEFINED __attribute__((no_sanitize("undefined")))
#elif defined(__GNUC__)
#define NO_SANITIZE_UNDEFINED __attribute__((no_sanitize_undefined))
#else
#define NO_SANITIZE_UNDEFINED
#endif

#ifdef SIMDJSON_VISUAL_STUDIO
// This is one case where we do not distinguish between
// regular visual studio and clang under visual studio.
// clang under Windows has _stricmp (like visual studio) but not strcasecmp (as clang normally has)
#define simdjson_strcasecmp _stricmp
#define simdjson_strncasecmp _strnicmp
#else
// The strcasecmp, strncasecmp, and strcasestr functions do not work with multibyte strings (e.g. UTF-8).
// So they are only useful for ASCII in our context.
// https://www.gnu.org/software/libunistring/manual/libunistring.html#char-_002a-strings
#define simdjson_strcasecmp strcasecmp
#define simdjson_strncasecmp strncasecmp
#endif

#ifdef NDEBUG

#ifdef SIMDJSON_VISUAL_STUDIO
#define SIMDJSON_UNREACHABLE() __assume(0)
#define SIMDJSON_ASSUME(COND) __assume(COND)
#else
#define SIMDJSON_UNREACHABLE() __builtin_unreachable();
#define SIMDJSON_ASSUME(COND) do { if (!(COND)) __builtin_unreachable(); } while (0)
#endif

#else // NDEBUG

#define SIMDJSON_UNREACHABLE() assert(0);
#define SIMDJSON_ASSUME(COND) assert(COND)

#endif

#endif // SIMDJSON_PORTABILITY_H
/* end file include/simdjson/portability.h */

namespace simdjson {

namespace internal {
/**
 * @private
 * Our own implementation of the C++17 to_chars function.
 * Defined in src/to_chars
 */
char *to_chars(char *first, const char *last, double value);
/**
 * @private
 * A number parsing routine.
 * Defined in src/from_chars
 */
double from_chars(const char *first) noexcept;
}

#ifndef SIMDJSON_EXCEPTIONS
#if __cpp_exceptions
#define SIMDJSON_EXCEPTIONS 1
#else
#define SIMDJSON_EXCEPTIONS 0
#endif
#endif

/** The maximum document size supported by simdjson. */
constexpr size_t SIMDJSON_MAXSIZE_BYTES = 0xFFFFFFFF;

/**
 * The amount of padding needed in a buffer to parse JSON.
 *
 * the input buf should be readable up to buf + SIMDJSON_PADDING
 * this is a stopgap; there should be a better description of the
 * main loop and its behavior that abstracts over this
 * See https://github.com/simdjson/simdjson/issues/174
 */
constexpr size_t SIMDJSON_PADDING = 32;

/**
 * By default, simdjson supports this many nested objects and arrays.
 *
 * This is the default for parser::max_depth().
 */
constexpr size_t DEFAULT_MAX_DEPTH = 1024;

} // namespace simdjson

#if defined(__GNUC__)
  // Marks a block with a name so that MCA analysis can see it.
  #define SIMDJSON_BEGIN_DEBUG_BLOCK(name) __asm volatile("# LLVM-MCA-BEGIN " #name);
  #define SIMDJSON_END_DEBUG_BLOCK(name) __asm volatile("# LLVM-MCA-END " #name);
  #define SIMDJSON_DEBUG_BLOCK(name, block) BEGIN_DEBUG_BLOCK(name); block; END_DEBUG_BLOCK(name);
#else
  #define SIMDJSON_BEGIN_DEBUG_BLOCK(name)
  #define SIMDJSON_END_DEBUG_BLOCK(name)
  #define SIMDJSON_DEBUG_BLOCK(name, block)
#endif

// Align to N-byte boundary
#define SIMDJSON_ROUNDUP_N(a, n) (((a) + ((n)-1)) & ~((n)-1))
#define SIMDJSON_ROUNDDOWN_N(a, n) ((a) & ~((n)-1))

#define SIMDJSON_ISALIGNED_N(ptr, n) (((uintptr_t)(ptr) & ((n)-1)) == 0)

#if defined(SIMDJSON_REGULAR_VISUAL_STUDIO)

  #define simdjson_really_inline __forceinline
  #define simdjson_never_inline __declspec(noinline)

  #define simdjson_unused
  #define simdjson_warn_unused

  #ifndef simdjson_likely
  #define simdjson_likely(x) x
  #endif
  #ifndef simdjson_unlikely
  #define simdjson_unlikely(x) x
  #endif

  #define SIMDJSON_PUSH_DISABLE_WARNINGS __pragma(warning( push ))
  #define SIMDJSON_PUSH_DISABLE_ALL_WARNINGS __pragma(warning( push, 0 ))
  #define SIMDJSON_DISABLE_VS_WARNING(WARNING_NUMBER) __pragma(warning( disable : WARNING_NUMBER ))
  // Get rid of Intellisense-only warnings (Code Analysis)
  // Though __has_include is C++17, it is supported in Visual Studio 2017 or better (_MSC_VER>=1910).
  #ifdef __has_include
  #if __has_include(<CppCoreCheck\Warnings.h>)
  #include <CppCoreCheck\Warnings.h>
  #define SIMDJSON_DISABLE_UNDESIRED_WARNINGS SIMDJSON_DISABLE_VS_WARNING(ALL_CPPCORECHECK_WARNINGS)
  #endif
  #endif

  #ifndef SIMDJSON_DISABLE_UNDESIRED_WARNINGS
  #define SIMDJSON_DISABLE_UNDESIRED_WARNINGS
  #endif

  #define SIMDJSON_DISABLE_DEPRECATED_WARNING SIMDJSON_DISABLE_VS_WARNING(4996)
  #define SIMDJSON_DISABLE_STRICT_OVERFLOW_WARNING
  #define SIMDJSON_POP_DISABLE_WARNINGS __pragma(warning( pop ))

#else // SIMDJSON_REGULAR_VISUAL_STUDIO

  #define simdjson_really_inline inline __attribute__((always_inline))
  #define simdjson_never_inline inline __attribute__((noinline))

  #define simdjson_unused __attribute__((unused))
  #define simdjson_warn_unused __attribute__((warn_unused_result))

  #ifndef simdjson_likely
  #define simdjson_likely(x) __builtin_expect(!!(x), 1)
  #endif
  #ifndef simdjson_unlikely
  #define simdjson_unlikely(x) __builtin_expect(!!(x), 0)
  #endif

  #define SIMDJSON_PUSH_DISABLE_WARNINGS _Pragma("GCC diagnostic push")
  // gcc doesn't seem to disable all warnings with all and extra, add warnings here as necessary
  #define SIMDJSON_PUSH_DISABLE_ALL_WARNINGS SIMDJSON_PUSH_DISABLE_WARNINGS \
    SIMDJSON_DISABLE_GCC_WARNING(-Weffc++) \
    SIMDJSON_DISABLE_GCC_WARNING(-Wall) \
    SIMDJSON_DISABLE_GCC_WARNING(-Wconversion) \
    SIMDJSON_DISABLE_GCC_WARNING(-Wextra) \
    SIMDJSON_DISABLE_GCC_WARNING(-Wattributes) \
    SIMDJSON_DISABLE_GCC_WARNING(-Wimplicit-fallthrough) \
    SIMDJSON_DISABLE_GCC_WARNING(-Wnon-virtual-dtor) \
    SIMDJSON_DISABLE_GCC_WARNING(-Wreturn-type) \
    SIMDJSON_DISABLE_GCC_WARNING(-Wshadow) \
    SIMDJSON_DISABLE_GCC_WARNING(-Wunused-parameter) \
    SIMDJSON_DISABLE_GCC_WARNING(-Wunused-variable)
  #define SIMDJSON_PRAGMA(P) _Pragma(#P)
  #define SIMDJSON_DISABLE_GCC_WARNING(WARNING) SIMDJSON_PRAGMA(GCC diagnostic ignored #WARNING)
  #if defined(SIMDJSON_CLANG_VISUAL_STUDIO)
  #define SIMDJSON_DISABLE_UNDESIRED_WARNINGS SIMDJSON_DISABLE_GCC_WARNING(-Wmicrosoft-include)
  #else
  #define SIMDJSON_DISABLE_UNDESIRED_WARNINGS
  #endif
  #define SIMDJSON_DISABLE_DEPRECATED_WARNING SIMDJSON_DISABLE_GCC_WARNING(-Wdeprecated-declarations)
  #define SIMDJSON_DISABLE_STRICT_OVERFLOW_WARNING SIMDJSON_DISABLE_GCC_WARNING(-Wstrict-overflow)
  #define SIMDJSON_POP_DISABLE_WARNINGS _Pragma("GCC diagnostic pop")



#endif // MSC_VER

#if defined(SIMDJSON_VISUAL_STUDIO)
    /**
     * Windows users need to do some extra work when building
     * or using a dynamic library (DLL). When building, we need
     * to set SIMDJSON_DLLIMPORTEXPORT to __declspec(dllexport).
     * When *using* the DLL, the user needs to set
     * SIMDJSON_DLLIMPORTEXPORT __declspec(dllimport).
     *
     * Static libraries not need require such work.
     *
     * It does not matter here whether you are using
     * the regular visual studio or clang under visual
     * studio, you still need to handle these issues.
     *
     * Non-Windows systems do not have this complexity.
     */
    #if SIMDJSON_BUILDING_WINDOWS_DYNAMIC_LIBRARY
    // We set SIMDJSON_BUILDING_WINDOWS_DYNAMIC_LIBRARY when we build a DLL under Windows.
    // It should never happen that both SIMDJSON_BUILDING_WINDOWS_DYNAMIC_LIBRARY and
    // SIMDJSON_USING_WINDOWS_DYNAMIC_LIBRARY are set.
    #define SIMDJSON_DLLIMPORTEXPORT __declspec(dllexport)
    #elif SIMDJSON_USING_WINDOWS_DYNAMIC_LIBRARY
    // Windows user who call a dynamic library should set SIMDJSON_USING_WINDOWS_DYNAMIC_LIBRARY to 1.
    #define SIMDJSON_DLLIMPORTEXPORT __declspec(dllimport)
    #else
    // We assume by default static linkage
    #define SIMDJSON_DLLIMPORTEXPORT
    #endif

/**
 * Workaround for the vcpkg package manager. Only vcpkg should
 * ever touch the next line. The SIMDJSON_USING_LIBRARY macro is otherwise unused.
 */
#if SIMDJSON_USING_LIBRARY
#define SIMDJSON_DLLIMPORTEXPORT __declspec(dllimport)
#endif
/**
 * End of workaround for the vcpkg package manager.
 */
#else
    #define SIMDJSON_DLLIMPORTEXPORT
#endif

// C++17 requires string_view.
#if SIMDJSON_CPLUSPLUS17
#define SIMDJSON_HAS_STRING_VIEW
#include <string_view> // by the standard, this has to be safe.
#endif

// This macro (__cpp_lib_string_view) has to be defined
// for C++17 and better, but if it is otherwise defined,
// we are going to assume that string_view is available
// even if we do not have C++17 support.
#ifdef __cpp_lib_string_view
#define SIMDJSON_HAS_STRING_VIEW
#endif

// Some systems have string_view even if we do not have C++17 support,
// and even if __cpp_lib_string_view is undefined, it is the case
// with Apple clang version 11.
// We must handle it. *This is important.*
#ifndef SIMDJSON_HAS_STRING_VIEW
#if defined __has_include
// do not combine the next #if with the previous one (unsafe)
#if __has_include (<string_view>)
// now it is safe to trigger the include
#include <string_view> // though the file is there, it does not follow that we got the implementation
#if defined(_LIBCPP_STRING_VIEW)
// Ah! So we under libc++ which under its Library Fundamentals Technical Specification, which preceded C++17,
// included string_view.
// This means that we have string_view *even though* we may not have C++17.
#define SIMDJSON_HAS_STRING_VIEW
#endif // _LIBCPP_STRING_VIEW
#endif // __has_include (<string_view>)
#endif // defined __has_include
#endif // def SIMDJSON_HAS_STRING_VIEW
// end of complicated but important routine to try to detect string_view.

//
// Backfill std::string_view using nonstd::string_view on systems where
// we expect that string_view is missing. Important: if we get this wrong,
// we will end up with two string_view definitions and potential trouble.
// That is why we work so hard above to avoid it.
//
#ifndef SIMDJSON_HAS_STRING_VIEW
SIMDJSON_PUSH_DISABLE_ALL_WARNINGS
#include "simdjson/nonstd/string_view.hpp"
SIMDJSON_POP_DISABLE_WARNINGS

namespace std {
  using string_view = nonstd::string_view;
}
#endif // SIMDJSON_HAS_STRING_VIEW
#undef SIMDJSON_HAS_STRING_VIEW // We are not going to need this macro anymore.

/// If EXPR is an error, returns it.
#define SIMDJSON_TRY(EXPR) { auto _err = (EXPR); if (_err) { return _err; } }

#ifndef SIMDJSON_DEVELOPMENT_CHECKS
#ifndef NDEBUG
#define SIMDJSON_DEVELOPMENT_CHECKS
#endif
#endif

#if SIMDJSON_CPLUSPLUS17
// if we have C++, then fallthrough is a default attribute
# define simdjson_fallthrough [[fallthrough]]
// check if we have __attribute__ support
#elif defined(__has_attribute)
// check if we have the __fallthrough__ attribute
#if __has_attribute(__fallthrough__)
// we are good to go:
# define simdjson_fallthrough                    __attribute__((__fallthrough__))
#endif
#endif
// on some systems, we simply do not have support for fallthrough, so use a default:
#ifndef simdjson_fallthrough
# define simdjson_fallthrough do {} while (0)  /* fallthrough */
#endif

#endif // SIMDJSON_COMMON_DEFS_H
/* end file include/simdjson/common_defs.h */

SIMDJSON_PUSH_DISABLE_WARNINGS
SIMDJSON_DISABLE_UNDESIRED_WARNINGS

// Public API
/* begin file include/simdjson/simdjson_version.h */
// /include/simdjson/simdjson_version.h automatically generated by release.py,
// do not change by hand
#ifndef SIMDJSON_SIMDJSON_VERSION_H
#define SIMDJSON_SIMDJSON_VERSION_H

/** The version of simdjson being used (major.minor.revision) */
#define SIMDJSON_VERSION 0.9.1

namespace simdjson {
enum {
  /**
   * The major version (MAJOR.minor.revision) of simdjson being used.
   */
  SIMDJSON_VERSION_MAJOR = 0,
  /**
   * The minor version (major.MINOR.revision) of simdjson being used.
   */
  SIMDJSON_VERSION_MINOR = 9,
  /**
   * The revision (major.minor.REVISION) of simdjson being used.
   */
  SIMDJSON_VERSION_REVISION = 1
};
} // namespace simdjson

#endif // SIMDJSON_SIMDJSON_VERSION_H
/* end file include/simdjson/simdjson_version.h */
/* begin file include/simdjson/error.h */
#ifndef SIMDJSON_ERROR_H
#define SIMDJSON_ERROR_H

namespace simdjson {

/**
 * All possible errors returned by validate_utf8.
 */
enum error_code {
  SUCCESS = 0,              ///< No error
  UTF8_ERROR                ///< the input is not valid UTF-8
};

} // namespace simdjson

#endif // SIMDJSON_ERROR_H
/* end file include/simdjson/error.h */
/* begin file include/simdjson/implementation.h */
#ifndef SIMDJSON_IMPLEMENTATION_H
#define SIMDJSON_IMPLEMENTATION_H

/* begin file include/simdjson/internal/isadetection.h */
/* From
https://github.com/endorno/pytorch/blob/master/torch/lib/TH/generic/simd/simd.h
Highly modified.

Copyright (c) 2016-     Facebook, Inc            (Adam Paszke)
Copyright (c) 2014-     Facebook, Inc            (Soumith Chintala)
Copyright (c) 2011-2014 Idiap Research Institute (Ronan Collobert)
Copyright (c) 2012-2014 Deepmind Technologies    (Koray Kavukcuoglu)
Copyright (c) 2011-2012 NEC Laboratories America (Koray Kavukcuoglu)
Copyright (c) 2011-2013 NYU                      (Clement Farabet)
Copyright (c) 2006-2010 NEC Laboratories America (Ronan Collobert, Leon Bottou,
Iain Melvin, Jason Weston) Copyright (c) 2006      Idiap Research Institute
(Samy Bengio) Copyright (c) 2001-2004 Idiap Research Institute (Ronan Collobert,
Samy Bengio, Johnny Mariethoz)

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the names of Facebook, Deepmind Technologies, NYU, NEC Laboratories
America and IDIAP Research Institute nor the names of its contributors may be
   used to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef SIMDJSON_INTERNAL_ISADETECTION_H
#define SIMDJSON_INTERNAL_ISADETECTION_H

#include <cstdint>
#include <cstdlib>
#if defined(_MSC_VER)
#include <intrin.h>
#elif defined(HAVE_GCC_GET_CPUID) && defined(USE_GCC_GET_CPUID)
#include <cpuid.h>
#endif

namespace simdjson {
namespace internal {


enum instruction_set {
  DEFAULT = 0x0,
  NEON = 0x1,
  AVX2 = 0x4,
  SSE42 = 0x8,
  PCLMULQDQ = 0x10,
  BMI1 = 0x20,
  BMI2 = 0x40,
  ALTIVEC = 0x80
};

#if defined(__PPC64__)

static inline uint32_t detect_supported_architectures() {
  return instruction_set::ALTIVEC;
}

#elif defined(__arm__) || defined(__aarch64__) // incl. armel, armhf, arm64

#if defined(__ARM_NEON)

static inline uint32_t detect_supported_architectures() {
  return instruction_set::NEON;
}

#else // ARM without NEON

static inline uint32_t detect_supported_architectures() {
  return instruction_set::DEFAULT;
}

#endif

#elif defined(__x86_64__) || defined(_M_AMD64) // x64


namespace {
// Can be found on Intel ISA Reference for CPUID
constexpr uint32_t cpuid_avx2_bit = 1 << 5;      ///< @private Bit 5 of EBX for EAX=0x7
constexpr uint32_t cpuid_bmi1_bit = 1 << 3;      ///< @private bit 3 of EBX for EAX=0x7
constexpr uint32_t cpuid_bmi2_bit = 1 << 8;      ///< @private bit 8 of EBX for EAX=0x7
constexpr uint32_t cpuid_sse42_bit = 1 << 20;    ///< @private bit 20 of ECX for EAX=0x1
constexpr uint32_t cpuid_pclmulqdq_bit = 1 << 1; ///< @private bit  1 of ECX for EAX=0x1
}



static inline void cpuid(uint32_t *eax, uint32_t *ebx, uint32_t *ecx,
                         uint32_t *edx) {
#if defined(_MSC_VER)
  int cpu_info[4];
  __cpuid(cpu_info, *eax);
  *eax = cpu_info[0];
  *ebx = cpu_info[1];
  *ecx = cpu_info[2];
  *edx = cpu_info[3];
#elif defined(HAVE_GCC_GET_CPUID) && defined(USE_GCC_GET_CPUID)
  uint32_t level = *eax;
  __get_cpuid(level, eax, ebx, ecx, edx);
#else
  uint32_t a = *eax, b, c = *ecx, d;
  asm volatile("cpuid\n\t" : "+a"(a), "=b"(b), "+c"(c), "=d"(d));
  *eax = a;
  *ebx = b;
  *ecx = c;
  *edx = d;
#endif
}

static inline uint32_t detect_supported_architectures() {
  uint32_t eax, ebx, ecx, edx;
  uint32_t host_isa = 0x0;

  // ECX for EAX=0x7
  eax = 0x7;
  ecx = 0x0;
  cpuid(&eax, &ebx, &ecx, &edx);
  if (ebx & cpuid_avx2_bit) {
    host_isa |= instruction_set::AVX2;
  }
  if (ebx & cpuid_bmi1_bit) {
    host_isa |= instruction_set::BMI1;
  }

  if (ebx & cpuid_bmi2_bit) {
    host_isa |= instruction_set::BMI2;
  }

  // EBX for EAX=0x1
  eax = 0x1;
  cpuid(&eax, &ebx, &ecx, &edx);

  if (ecx & cpuid_sse42_bit) {
    host_isa |= instruction_set::SSE42;
  }

  if (ecx & cpuid_pclmulqdq_bit) {
    host_isa |= instruction_set::PCLMULQDQ;
  }

  return host_isa;
}
#else // fallback


static inline uint32_t detect_supported_architectures() {
  return instruction_set::DEFAULT;
}


#endif // end SIMD extension detection code

} // namespace internal
} // namespace simdjson

#endif // SIMDJSON_INTERNAL_ISADETECTION_H
/* end file include/simdjson/internal/isadetection.h */
#include <string>
#include <atomic>
#include <vector>

namespace simdjson {

/**
 * Validate the UTF-8 string.
 *
 * @param buf the string to validate.
 * @param len the length of the string in bytes.
 * @return true if the string is valid UTF-8.
 */
simdjson_warn_unused bool validate_utf8(const char * buf, size_t len) noexcept;


/**
 * Validate the UTF-8 string.
 *
 * @param sv the string_view to validate.
 * @return true if the string is valid UTF-8.
 */
simdjson_really_inline simdjson_warn_unused bool validate_utf8(const std::string_view sv) noexcept {
  return validate_utf8(sv.data(), sv.size());
}

/**
 * Validate the UTF-8 string.
 *
 * @param p the string to validate.
 * @return true if the string is valid UTF-8.
 */
simdjson_really_inline simdjson_warn_unused bool validate_utf8(const std::string& s) noexcept {
  return validate_utf8(s.data(), s.size());
}

/**
 * An implementation of simdjson for a particular CPU architecture.
 *
 * Also used to maintain the currently active implementation. The active implementation is
 * automatically initialized on first use to the most advanced implementation supported by the host.
 */
class implementation {
public:

  /**
   * The name of this implementation.
   *
   *     const implementation *impl = simdjson::active_implementation;
   *     cout << "simdjson is optimized for " << impl->name() << "(" << impl->description() << ")" << endl;
   *
   * @return the name of the implementation, e.g. "haswell", "westmere", "arm64"
   */
  virtual const std::string &name() const { return _name; }

  /**
   * The description of this implementation.
   *
   *     const implementation *impl = simdjson::active_implementation;
   *     cout << "simdjson is optimized for " << impl->name() << "(" << impl->description() << ")" << endl;
   *
   * @return the name of the implementation, e.g. "haswell", "westmere", "arm64"
   */
  virtual const std::string &description() const { return _description; }

  /**
   * The instruction sets this implementation is compiled against
   * and the current CPU match. This function may poll the current CPU/system
   * and should therefore not be called too often if performance is a concern.
   *
   *
   * @return true if the implementation can be safely used on the current system (determined at runtime)
   */
  bool supported_by_runtime_system() const;

  /**
   * @private For internal implementation use
   *
   * The instruction sets this implementation is compiled against.
   *
   * @return a mask of all required `internal::instruction_set::` values
   */
  virtual uint32_t required_instruction_sets() const { return _required_instruction_sets; };

  /**
   * Validate the UTF-8 string.
   *
   * Overridden by each implementation.
   *
   * @param buf the string to validate.
   * @param len the length of the string in bytes.
   * @return true if and only if the string is valid UTF-8.
   */
  simdjson_warn_unused virtual bool validate_utf8(const char *buf, size_t len) const noexcept = 0;

protected:
  /** @private Construct an implementation with the given name and description. For subclasses. */
  simdjson_really_inline implementation(
    std::string_view name,
    std::string_view description,
    uint32_t required_instruction_sets
  ) :
    _name(name),
    _description(description),
    _required_instruction_sets(required_instruction_sets)
  {
  }
  virtual ~implementation()=default;

private:
  /**
   * The name of this implementation.
   */
  const std::string _name;

  /**
   * The description of this implementation.
   */
  const std::string _description;

  /**
   * Instruction sets required for this implementation.
   */
  const uint32_t _required_instruction_sets;
};

/** @private */
namespace internal {

/**
 * The list of available implementations compiled into simdjson.
 */
class available_implementation_list {
public:
  /** Get the list of available implementations compiled into simdjson */
  simdjson_really_inline available_implementation_list() {}
  /** Number of implementations */
  size_t size() const noexcept;
  /** STL const begin() iterator */
  const implementation * const *begin() const noexcept;
  /** STL const end() iterator */
  const implementation * const *end() const noexcept;

  /**
   * Get the implementation with the given name.
   *
   * Case sensitive.
   *
   *     const implementation *impl = simdjson::available_implementations["westmere"];
   *     if (!impl) { exit(1); }
   *     if (!imp->supported_by_runtime_system()) { exit(1); }
   *     simdjson::active_implementation = impl;
   *
   * @param name the implementation to find, e.g. "westmere", "haswell", "arm64"
   * @return the implementation, or nullptr if the parse failed.
   */
  const implementation * operator[](const std::string_view &name) const noexcept {
    for (const implementation * impl : *this) {
      if (impl->name() == name) { return impl; }
    }
    return nullptr;
  }

  /**
   * Detect the most advanced implementation supported by the current host.
   *
   * This is used to initialize the implementation on startup.
   *
   *     const implementation *impl = simdjson::available_implementation::detect_best_supported();
   *     simdjson::active_implementation = impl;
   *
   * @return the most advanced supported implementation for the current host, or an
   *         implementation that returns UNSUPPORTED_ARCHITECTURE if there is no supported
   *         implementation. Will never return nullptr.
   */
  const implementation *detect_best_supported() const noexcept;
};

template<typename T>
class atomic_ptr {
public:
  atomic_ptr(T *_ptr) : ptr{_ptr} {}

  operator const T*() const { return ptr.load(); }
  const T& operator*() const { return *ptr; }
  const T* operator->() const { return ptr.load(); }

  operator T*() { return ptr.load(); }
  T& operator*() { return *ptr; }
  T* operator->() { return ptr.load(); }
  atomic_ptr& operator=(T *_ptr) { ptr = _ptr; return *this; }

private:
  std::atomic<T*> ptr;
};

} // namespace internal

/**
 * The list of available implementations compiled into simdjson.
 */
extern SIMDJSON_DLLIMPORTEXPORT const internal::available_implementation_list available_implementations;

/**
  * The active implementation.
  *
  * Automatically initialized on first use to the most advanced implementation supported by this hardware.
  */
extern SIMDJSON_DLLIMPORTEXPORT internal::atomic_ptr<const implementation> active_implementation;

} // namespace simdjson

#endif // SIMDJSON_IMPLEMENTATION_H
/* end file include/simdjson/implementation.h */

SIMDJSON_POP_DISABLE_WARNINGS

#endif // SIMDJSON_BASE_H
/* end file include/simdjson/base.h */

// Implementation-internal files (must be included before the implementations themselves, to keep
// amalgamation working--otherwise, the first time a file is included, it might be put inside the
// #ifdef SIMDJSON_IMPLEMENTATION_ARM64/FALLBACK/etc., which means the other implementations can't
// compile unless that implementation is turned on).
/* begin file include/simdjson/internal/simdprune_tables.h */
#ifndef SIMDJSON_INTERNAL_SIMDPRUNE_TABLES_H
#define SIMDJSON_INTERNAL_SIMDPRUNE_TABLES_H

#include <cstdint>

namespace simdjson { // table modified and copied from
namespace internal { // http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetTable

extern SIMDJSON_DLLIMPORTEXPORT const unsigned char BitsSetTable256mul2[256];

extern SIMDJSON_DLLIMPORTEXPORT const uint8_t pshufb_combine_table[272];

// 256 * 8 bytes = 2kB, easily fits in cache.
extern SIMDJSON_DLLIMPORTEXPORT const uint64_t thintable_epi8[256];

} // namespace internal
} // namespace simdjson

#endif // SIMDJSON_INTERNAL_SIMDPRUNE_TABLES_H
/* end file include/simdjson/internal/simdprune_tables.h */

#endif // SIMDJSON_IMPLEMENTATION_BASE_H
/* end file include/simdjson/implementation-base.h */

//
// First, figure out which implementations can be run. Doing it here makes it so we don't have to worry about the order
// in which we include them.
//

#ifndef SIMDJSON_IMPLEMENTATION_ARM64
#define SIMDJSON_IMPLEMENTATION_ARM64 (SIMDJSON_IS_ARM64)
#endif
#define SIMDJSON_CAN_ALWAYS_RUN_ARM64 SIMDJSON_IMPLEMENTATION_ARM64 && SIMDJSON_IS_ARM64

// Default Haswell to on if this is x86-64. Even if we're not compiled for it, it could be selected
// at runtime.
#ifndef SIMDJSON_IMPLEMENTATION_HASWELL
#define SIMDJSON_IMPLEMENTATION_HASWELL (SIMDJSON_IS_X86_64)
#endif
// To see why  (__BMI__) && (__PCLMUL__) && (__LZCNT__) are not part of this next line, see
// https://github.com/simdjson/simdjson/issues/1247
#define SIMDJSON_CAN_ALWAYS_RUN_HASWELL ((SIMDJSON_IMPLEMENTATION_HASWELL) && (SIMDJSON_IS_X86_64) && (__AVX2__))

// Default Westmere to on if this is x86-64, unless we'll always select Haswell.
#ifndef SIMDJSON_IMPLEMENTATION_WESTMERE
#define SIMDJSON_IMPLEMENTATION_WESTMERE (SIMDJSON_IS_X86_64 && !SIMDJSON_REQUIRES_HASWELL)
#endif
#define SIMDJSON_CAN_ALWAYS_RUN_WESTMERE (SIMDJSON_IMPLEMENTATION_WESTMERE && SIMDJSON_IS_X86_64 && __SSE4_2__ && __PCLMUL__)

#ifndef SIMDJSON_IMPLEMENTATION_PPC64
#define SIMDJSON_IMPLEMENTATION_PPC64 (SIMDJSON_IS_PPC64)
#endif
#define SIMDJSON_CAN_ALWAYS_RUN_PPC64 SIMDJSON_IMPLEMENTATION_PPC64 && SIMDJSON_IS_PPC64

// Default Fallback to on unless a builtin implementation has already been selected.
#ifndef SIMDJSON_IMPLEMENTATION_FALLBACK
#define SIMDJSON_IMPLEMENTATION_FALLBACK 1 // (!SIMDJSON_CAN_ALWAYS_RUN_ARM64 && !SIMDJSON_CAN_ALWAYS_RUN_HASWELL && !SIMDJSON_CAN_ALWAYS_RUN_WESTMERE && !SIMDJSON_CAN_ALWAYS_RUN_PPC64)
#endif
#define SIMDJSON_CAN_ALWAYS_RUN_FALLBACK SIMDJSON_IMPLEMENTATION_FALLBACK

SIMDJSON_PUSH_DISABLE_WARNINGS
SIMDJSON_DISABLE_UNDESIRED_WARNINGS

// Implementations
/* begin file include/simdjson/arm64.h */
#ifndef SIMDJSON_ARM64_H
#define SIMDJSON_ARM64_H


#if SIMDJSON_IMPLEMENTATION_ARM64

namespace simdjson {
/**
 * Implementation for NEON (ARMv8).
 */
namespace arm64 {
} // namespace arm64
} // namespace simdjson

/* begin file include/simdjson/arm64/implementation.h */
#ifndef SIMDJSON_ARM64_IMPLEMENTATION_H
#define SIMDJSON_ARM64_IMPLEMENTATION_H


namespace simdjson {
namespace arm64 {

namespace {
using namespace simdjson;
using namespace simdjson::dom;
}

class implementation final : public simdjson::implementation {
public:
  simdjson_really_inline implementation() : simdjson::implementation("arm64", "ARM NEON", internal::instruction_set::NEON) {}
  simdjson_warn_unused bool validate_utf8(const char *buf, size_t len) const noexcept final;
};

} // namespace arm64
} // namespace simdjson

#endif // SIMDJSON_ARM64_IMPLEMENTATION_H
/* end file include/simdjson/arm64/implementation.h */

/* begin file include/simdjson/arm64/begin.h */
// redefining SIMDJSON_IMPLEMENTATION to "arm64"
// #define SIMDJSON_IMPLEMENTATION arm64
/* end file include/simdjson/arm64/begin.h */

// Declarations
/* begin file include/simdjson/arm64/intrinsics.h */
#ifndef SIMDJSON_ARM64_INTRINSICS_H
#define SIMDJSON_ARM64_INTRINSICS_H

// This should be the correct header whether
// you use visual studio or other compilers.
#include <arm_neon.h>

#endif //  SIMDJSON_ARM64_INTRINSICS_H
/* end file include/simdjson/arm64/intrinsics.h */
/* begin file include/simdjson/arm64/bitmanipulation.h */
#ifndef SIMDJSON_ARM64_BITMANIPULATION_H
#define SIMDJSON_ARM64_BITMANIPULATION_H

namespace simdjson {
namespace arm64 {
namespace {

// We sometimes call trailing_zero on inputs that are zero,
// but the algorithms do not end up using the returned value.
// Sadly, sanitizers are not smart enough to figure it out.
NO_SANITIZE_UNDEFINED
simdjson_really_inline int trailing_zeroes(uint64_t input_num) {
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
  unsigned long ret;
  // Search the mask data from least significant bit (LSB)
  // to the most significant bit (MSB) for a set bit (1).
  _BitScanForward64(&ret, input_num);
  return (int)ret;
#else // SIMDJSON_REGULAR_VISUAL_STUDIO
  return __builtin_ctzll(input_num);
#endif // SIMDJSON_REGULAR_VISUAL_STUDIO
}

/* result might be undefined when input_num is zero */
simdjson_really_inline uint64_t clear_lowest_bit(uint64_t input_num) {
  return input_num & (input_num-1);
}

/* result might be undefined when input_num is zero */
simdjson_really_inline int leading_zeroes(uint64_t input_num) {
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
  unsigned long leading_zero = 0;
  // Search the mask data from most significant bit (MSB)
  // to least significant bit (LSB) for a set bit (1).
  if (_BitScanReverse64(&leading_zero, input_num))
    return (int)(63 - leading_zero);
  else
    return 64;
#else
  return __builtin_clzll(input_num);
#endif// SIMDJSON_REGULAR_VISUAL_STUDIO
}

/* result might be undefined when input_num is zero */
simdjson_really_inline int count_ones(uint64_t input_num) {
   return vaddv_u8(vcnt_u8(vcreate_u8(input_num)));
}


#if defined(__GNUC__) // catches clang and gcc
/**
 * ARM has a fast 64-bit "bit reversal function" that is handy. However,
 * it is not generally available as an intrinsic function under Visual
 * Studio (though this might be changing). Even under clang/gcc, we
 * apparently need to invoke inline assembly.
 */
/*
 * We use SIMDJSON_PREFER_REVERSE_BITS as a hint that algorithms that
 * work well with bit reversal may use it.
 */
#define SIMDJSON_PREFER_REVERSE_BITS 1

/* reverse the bits */
simdjson_really_inline uint64_t reverse_bits(uint64_t input_num) {
  uint64_t rev_bits;
  __asm("rbit %0, %1" : "=r"(rev_bits) : "r"(input_num));
  return rev_bits;
}

/**
 * Flips bit at index 63 - lz. Thus if you have 'leading_zeroes' leading zeroes,
 * then this will set to zero the leading bit. It is possible for leading_zeroes to be
 * greating or equal to 63 in which case we trigger undefined behavior, but the output
 * of such undefined behavior is never used.
 **/
NO_SANITIZE_UNDEFINED
simdjson_really_inline uint64_t zero_leading_bit(uint64_t rev_bits, int leading_zeroes) {
  return rev_bits ^ (uint64_t(0x8000000000000000) >> leading_zeroes);
}

#endif

simdjson_really_inline bool add_overflow(uint64_t value1, uint64_t value2, uint64_t *result) {
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
  *result = value1 + value2;
  return *result < value1;
#else
  return __builtin_uaddll_overflow(value1, value2,
                                   reinterpret_cast<unsigned long long *>(result));
#endif
}

} // unnamed namespace
} // namespace arm64
} // namespace simdjson

#endif // SIMDJSON_ARM64_BITMANIPULATION_H
/* end file include/simdjson/arm64/bitmanipulation.h */
/* begin file include/simdjson/arm64/simd.h */
#ifndef SIMDJSON_ARM64_SIMD_H
#define SIMDJSON_ARM64_SIMD_H

#include <type_traits>


namespace simdjson {
namespace arm64 {
namespace {
namespace simd {

#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
namespace {
// Start of private section with Visual Studio workaround


/**
 * make_uint8x16_t initializes a SIMD register (uint8x16_t).
 * This is needed because, incredibly, the syntax uint8x16_t x = {1,2,3...}
 * is not recognized under Visual Studio! This is a workaround.
 * Using a std::initializer_list<uint8_t>  as a parameter resulted in
 * inefficient code. With the current approach, if the parameters are
 * compile-time constants,
 * GNU GCC compiles it to ldr, the same as uint8x16_t x = {1,2,3...}.
 * You should not use this function except for compile-time constants:
 * it is not efficient.
 */
simdjson_really_inline uint8x16_t make_uint8x16_t(uint8_t x1,  uint8_t x2,  uint8_t x3,  uint8_t x4,
                                         uint8_t x5,  uint8_t x6,  uint8_t x7,  uint8_t x8,
                                         uint8_t x9,  uint8_t x10, uint8_t x11, uint8_t x12,
                                         uint8_t x13, uint8_t x14, uint8_t x15, uint8_t x16) {
  // Doing a load like so end ups generating worse code.
  // uint8_t array[16] = {x1, x2, x3, x4, x5, x6, x7, x8,
  //                     x9, x10,x11,x12,x13,x14,x15,x16};
  // return vld1q_u8(array);
  uint8x16_t x{};
  // incredibly, Visual Studio does not allow x[0] = x1
  x = vsetq_lane_u8(x1, x, 0);
  x = vsetq_lane_u8(x2, x, 1);
  x = vsetq_lane_u8(x3, x, 2);
  x = vsetq_lane_u8(x4, x, 3);
  x = vsetq_lane_u8(x5, x, 4);
  x = vsetq_lane_u8(x6, x, 5);
  x = vsetq_lane_u8(x7, x, 6);
  x = vsetq_lane_u8(x8, x, 7);
  x = vsetq_lane_u8(x9, x, 8);
  x = vsetq_lane_u8(x10, x, 9);
  x = vsetq_lane_u8(x11, x, 10);
  x = vsetq_lane_u8(x12, x, 11);
  x = vsetq_lane_u8(x13, x, 12);
  x = vsetq_lane_u8(x14, x, 13);
  x = vsetq_lane_u8(x15, x, 14);
  x = vsetq_lane_u8(x16, x, 15);
  return x;
}


// We have to do the same work for make_int8x16_t
simdjson_really_inline int8x16_t make_int8x16_t(int8_t x1,  int8_t x2,  int8_t x3,  int8_t x4,
                                       int8_t x5,  int8_t x6,  int8_t x7,  int8_t x8,
                                       int8_t x9,  int8_t x10, int8_t x11, int8_t x12,
                                       int8_t x13, int8_t x14, int8_t x15, int8_t x16) {
  // Doing a load like so end ups generating worse code.
  // int8_t array[16] = {x1, x2, x3, x4, x5, x6, x7, x8,
  //                     x9, x10,x11,x12,x13,x14,x15,x16};
  // return vld1q_s8(array);
  int8x16_t x{};
  // incredibly, Visual Studio does not allow x[0] = x1
  x = vsetq_lane_s8(x1, x, 0);
  x = vsetq_lane_s8(x2, x, 1);
  x = vsetq_lane_s8(x3, x, 2);
  x = vsetq_lane_s8(x4, x, 3);
  x = vsetq_lane_s8(x5, x, 4);
  x = vsetq_lane_s8(x6, x, 5);
  x = vsetq_lane_s8(x7, x, 6);
  x = vsetq_lane_s8(x8, x, 7);
  x = vsetq_lane_s8(x9, x, 8);
  x = vsetq_lane_s8(x10, x, 9);
  x = vsetq_lane_s8(x11, x, 10);
  x = vsetq_lane_s8(x12, x, 11);
  x = vsetq_lane_s8(x13, x, 12);
  x = vsetq_lane_s8(x14, x, 13);
  x = vsetq_lane_s8(x15, x, 14);
  x = vsetq_lane_s8(x16, x, 15);
  return x;
}

// End of private section with Visual Studio workaround
} // namespace
#endif // SIMDJSON_REGULAR_VISUAL_STUDIO


  template<typename T>
  struct simd8;

  //
  // Base class of simd8<uint8_t> and simd8<bool>, both of which use uint8x16_t internally.
  //
  template<typename T, typename Mask=simd8<bool>>
  struct base_u8 {
    uint8x16_t value;
    static const int SIZE = sizeof(value);

    // Conversion from/to SIMD register
    simdjson_really_inline base_u8(const uint8x16_t _value) : value(_value) {}
    simdjson_really_inline operator const uint8x16_t&() const { return this->value; }
    simdjson_really_inline operator uint8x16_t&() { return this->value; }

    // Bit operations
    simdjson_really_inline simd8<T> operator|(const simd8<T> other) const { return vorrq_u8(*this, other); }
    simdjson_really_inline simd8<T> operator&(const simd8<T> other) const { return vandq_u8(*this, other); }
    simdjson_really_inline simd8<T> operator^(const simd8<T> other) const { return veorq_u8(*this, other); }
    simdjson_really_inline simd8<T> bit_andnot(const simd8<T> other) const { return vbicq_u8(*this, other); }
    simdjson_really_inline simd8<T> operator~() const { return *this ^ 0xFFu; }
    simdjson_really_inline simd8<T>& operator|=(const simd8<T> other) { auto this_cast = static_cast<simd8<T>*>(this); *this_cast = *this_cast | other; return *this_cast; }
    simdjson_really_inline simd8<T>& operator&=(const simd8<T> other) { auto this_cast = static_cast<simd8<T>*>(this); *this_cast = *this_cast & other; return *this_cast; }
    simdjson_really_inline simd8<T>& operator^=(const simd8<T> other) { auto this_cast = static_cast<simd8<T>*>(this); *this_cast = *this_cast ^ other; return *this_cast; }

    simdjson_really_inline Mask operator==(const simd8<T> other) const { return vceqq_u8(*this, other); }

    template<int N=1>
    simdjson_really_inline simd8<T> prev(const simd8<T> prev_chunk) const {
      return vextq_u8(prev_chunk, *this, 16 - N);
    }
  };

  // SIMD byte mask type (returned by things like eq and gt)
  template<>
  struct simd8<bool>: base_u8<bool> {
    typedef uint16_t bitmask_t;
    typedef uint32_t bitmask2_t;

    static simdjson_really_inline simd8<bool> splat(bool _value) { return vmovq_n_u8(uint8_t(-(!!_value))); }

    simdjson_really_inline simd8(const uint8x16_t _value) : base_u8<bool>(_value) {}
    // False constructor
    simdjson_really_inline simd8() : simd8(vdupq_n_u8(0)) {}
    // Splat constructor
    simdjson_really_inline simd8(bool _value) : simd8(splat(_value)) {}

    // We return uint32_t instead of uint16_t because that seems to be more efficient for most
    // purposes (cutting it down to uint16_t costs performance in some compilers).
    simdjson_really_inline uint32_t to_bitmask() const {
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
      const uint8x16_t bit_mask =  make_uint8x16_t(0x01, 0x02, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80,
                                                   0x01, 0x02, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80);
#else
      const uint8x16_t bit_mask =  {0x01, 0x02, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80,
                                    0x01, 0x02, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80};
#endif
      auto minput = *this & bit_mask;
      uint8x16_t tmp = vpaddq_u8(minput, minput);
      tmp = vpaddq_u8(tmp, tmp);
      tmp = vpaddq_u8(tmp, tmp);
      return vgetq_lane_u16(vreinterpretq_u16_u8(tmp), 0);
    }
    simdjson_really_inline bool any() const { return vmaxvq_u8(*this) != 0; }
  };

  // Unsigned bytes
  template<>
  struct simd8<uint8_t>: base_u8<uint8_t> {
    static simdjson_really_inline uint8x16_t splat(uint8_t _value) { return vmovq_n_u8(_value); }
    static simdjson_really_inline uint8x16_t zero() { return vdupq_n_u8(0); }
    static simdjson_really_inline uint8x16_t load(const uint8_t* values) { return vld1q_u8(values); }

    simdjson_really_inline simd8(const uint8x16_t _value) : base_u8<uint8_t>(_value) {}
    // Zero constructor
    simdjson_really_inline simd8() : simd8(zero()) {}
    // Array constructor
    simdjson_really_inline simd8(const uint8_t values[16]) : simd8(load(values)) {}
    // Splat constructor
    simdjson_really_inline simd8(uint8_t _value) : simd8(splat(_value)) {}
    // Member-by-member initialization
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
    simdjson_really_inline simd8(
      uint8_t v0,  uint8_t v1,  uint8_t v2,  uint8_t v3,  uint8_t v4,  uint8_t v5,  uint8_t v6,  uint8_t v7,
      uint8_t v8,  uint8_t v9,  uint8_t v10, uint8_t v11, uint8_t v12, uint8_t v13, uint8_t v14, uint8_t v15
    ) : simd8(make_uint8x16_t(
      v0, v1, v2, v3, v4, v5, v6, v7,
      v8, v9, v10,v11,v12,v13,v14,v15
    )) {}
#else
    simdjson_really_inline simd8(
      uint8_t v0,  uint8_t v1,  uint8_t v2,  uint8_t v3,  uint8_t v4,  uint8_t v5,  uint8_t v6,  uint8_t v7,
      uint8_t v8,  uint8_t v9,  uint8_t v10, uint8_t v11, uint8_t v12, uint8_t v13, uint8_t v14, uint8_t v15
    ) : simd8(uint8x16_t{
      v0, v1, v2, v3, v4, v5, v6, v7,
      v8, v9, v10,v11,v12,v13,v14,v15
    }) {}
#endif

    // Repeat 16 values as many times as necessary (usually for lookup tables)
    simdjson_really_inline static simd8<uint8_t> repeat_16(
      uint8_t v0,  uint8_t v1,  uint8_t v2,  uint8_t v3,  uint8_t v4,  uint8_t v5,  uint8_t v6,  uint8_t v7,
      uint8_t v8,  uint8_t v9,  uint8_t v10, uint8_t v11, uint8_t v12, uint8_t v13, uint8_t v14, uint8_t v15
    ) {
      return simd8<uint8_t>(
        v0, v1, v2, v3, v4, v5, v6, v7,
        v8, v9, v10,v11,v12,v13,v14,v15
      );
    }

    // Store to array
    simdjson_really_inline void store(uint8_t dst[16]) const { return vst1q_u8(dst, *this); }

    // Saturated math
    simdjson_really_inline simd8<uint8_t> saturating_add(const simd8<uint8_t> other) const { return vqaddq_u8(*this, other); }
    simdjson_really_inline simd8<uint8_t> saturating_sub(const simd8<uint8_t> other) const { return vqsubq_u8(*this, other); }

    // Addition/subtraction are the same for signed and unsigned
    simdjson_really_inline simd8<uint8_t> operator+(const simd8<uint8_t> other) const { return vaddq_u8(*this, other); }
    simdjson_really_inline simd8<uint8_t> operator-(const simd8<uint8_t> other) const { return vsubq_u8(*this, other); }
    simdjson_really_inline simd8<uint8_t>& operator+=(const simd8<uint8_t> other) { *this = *this + other; return *this; }
    simdjson_really_inline simd8<uint8_t>& operator-=(const simd8<uint8_t> other) { *this = *this - other; return *this; }

    // Order-specific operations
    simdjson_really_inline uint8_t max_val() const { return vmaxvq_u8(*this); }
    simdjson_really_inline uint8_t min_val() const { return vminvq_u8(*this); }
    simdjson_really_inline simd8<uint8_t> max_val(const simd8<uint8_t> other) const { return vmaxq_u8(*this, other); }
    simdjson_really_inline simd8<uint8_t> min_val(const simd8<uint8_t> other) const { return vminq_u8(*this, other); }
    simdjson_really_inline simd8<bool> operator<=(const simd8<uint8_t> other) const { return vcleq_u8(*this, other); }
    simdjson_really_inline simd8<bool> operator>=(const simd8<uint8_t> other) const { return vcgeq_u8(*this, other); }
    simdjson_really_inline simd8<bool> operator<(const simd8<uint8_t> other) const { return vcltq_u8(*this, other); }
    simdjson_really_inline simd8<bool> operator>(const simd8<uint8_t> other) const { return vcgtq_u8(*this, other); }
    // Same as >, but instead of guaranteeing all 1's == true, false = 0 and true = nonzero. For ARM, returns all 1's.
    simdjson_really_inline simd8<uint8_t> gt_bits(const simd8<uint8_t> other) const { return simd8<uint8_t>(*this > other); }
    // Same as <, but instead of guaranteeing all 1's == true, false = 0 and true = nonzero. For ARM, returns all 1's.
    simdjson_really_inline simd8<uint8_t> lt_bits(const simd8<uint8_t> other) const { return simd8<uint8_t>(*this < other); }

    // Bit-specific operations
    simdjson_really_inline simd8<bool> any_bits_set(simd8<uint8_t> bits) const { return vtstq_u8(*this, bits); }
    simdjson_really_inline bool any_bits_set_anywhere() const { return this->max_val() != 0; }
    simdjson_really_inline bool any_bits_set_anywhere(simd8<uint8_t> bits) const { return (*this & bits).any_bits_set_anywhere(); }
    template<int N>
    simdjson_really_inline simd8<uint8_t> shr() const { return vshrq_n_u8(*this, N); }
    template<int N>
    simdjson_really_inline simd8<uint8_t> shl() const { return vshlq_n_u8(*this, N); }

    // Perform a lookup assuming the value is between 0 and 16 (undefined behavior for out of range values)
    template<typename L>
    simdjson_really_inline simd8<L> lookup_16(simd8<L> lookup_table) const {
      return lookup_table.apply_lookup_16_to(*this);
    }


    // Copies to 'output" all bytes corresponding to a 0 in the mask (interpreted as a bitset).
    // Passing a 0 value for mask would be equivalent to writing out every byte to output.
    // Only the first 16 - count_ones(mask) bytes of the result are significant but 16 bytes
    // get written.
    // Design consideration: it seems like a function with the
    // signature simd8<L> compress(uint16_t mask) would be
    // sensible, but the AVX ISA makes this kind of approach difficult.
    template<typename L>
    simdjson_really_inline void compress(uint16_t mask, L * output) const {
      using internal::thintable_epi8;
      using internal::BitsSetTable256mul2;
      using internal::pshufb_combine_table;
      // this particular implementation was inspired by work done by @animetosho
      // we do it in two steps, first 8 bytes and then second 8 bytes
      uint8_t mask1 = uint8_t(mask); // least significant 8 bits
      uint8_t mask2 = uint8_t(mask >> 8); // most significant 8 bits
      // next line just loads the 64-bit values thintable_epi8[mask1] and
      // thintable_epi8[mask2] into a 128-bit register, using only
      // two instructions on most compilers.
      uint64x2_t shufmask64 = {thintable_epi8[mask1], thintable_epi8[mask2]};
      uint8x16_t shufmask = vreinterpretq_u8_u64(shufmask64);
      // we increment by 0x08 the second half of the mask
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
      uint8x16_t inc = make_uint8x16_t(0, 0, 0, 0, 0, 0, 0, 0, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08);
#else
      uint8x16_t inc = {0, 0, 0, 0, 0, 0, 0, 0, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08};
#endif
      shufmask = vaddq_u8(shufmask, inc);
      // this is the version "nearly pruned"
      uint8x16_t pruned = vqtbl1q_u8(*this, shufmask);
      // we still need to put the two halves together.
      // we compute the popcount of the first half:
      int pop1 = BitsSetTable256mul2[mask1];
      // then load the corresponding mask, what it does is to write
      // only the first pop1 bytes from the first 8 bytes, and then
      // it fills in with the bytes from the second 8 bytes + some filling
      // at the end.
      uint8x16_t compactmask = vld1q_u8(reinterpret_cast<const uint8_t *>(pshufb_combine_table + pop1 * 8));
      uint8x16_t answer = vqtbl1q_u8(pruned, compactmask);
      vst1q_u8(reinterpret_cast<uint8_t*>(output), answer);
    }

    template<typename L>
    simdjson_really_inline simd8<L> lookup_16(
        L replace0,  L replace1,  L replace2,  L replace3,
        L replace4,  L replace5,  L replace6,  L replace7,
        L replace8,  L replace9,  L replace10, L replace11,
        L replace12, L replace13, L replace14, L replace15) const {
      return lookup_16(simd8<L>::repeat_16(
        replace0,  replace1,  replace2,  replace3,
        replace4,  replace5,  replace6,  replace7,
        replace8,  replace9,  replace10, replace11,
        replace12, replace13, replace14, replace15
      ));
    }

    template<typename T>
    simdjson_really_inline simd8<uint8_t> apply_lookup_16_to(const simd8<T> original) {
      return vqtbl1q_u8(*this, simd8<uint8_t>(original));
    }
  };

  // Signed bytes
  template<>
  struct simd8<int8_t> {
    int8x16_t value;

    static simdjson_really_inline simd8<int8_t> splat(int8_t _value) { return vmovq_n_s8(_value); }
    static simdjson_really_inline simd8<int8_t> zero() { return vdupq_n_s8(0); }
    static simdjson_really_inline simd8<int8_t> load(const int8_t values[16]) { return vld1q_s8(values); }

    // Conversion from/to SIMD register
    simdjson_really_inline simd8(const int8x16_t _value) : value{_value} {}
    simdjson_really_inline operator const int8x16_t&() const { return this->value; }
    simdjson_really_inline operator int8x16_t&() { return this->value; }

    // Zero constructor
    simdjson_really_inline simd8() : simd8(zero()) {}
    // Splat constructor
    simdjson_really_inline simd8(int8_t _value) : simd8(splat(_value)) {}
    // Array constructor
    simdjson_really_inline simd8(const int8_t* values) : simd8(load(values)) {}
    // Member-by-member initialization
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
    simdjson_really_inline simd8(
      int8_t v0,  int8_t v1,  int8_t v2,  int8_t v3, int8_t v4,  int8_t v5,  int8_t v6,  int8_t v7,
      int8_t v8,  int8_t v9,  int8_t v10, int8_t v11, int8_t v12, int8_t v13, int8_t v14, int8_t v15
    ) : simd8(make_int8x16_t(
      v0, v1, v2, v3, v4, v5, v6, v7,
      v8, v9, v10,v11,v12,v13,v14,v15
    )) {}
#else
    simdjson_really_inline simd8(
      int8_t v0,  int8_t v1,  int8_t v2,  int8_t v3, int8_t v4,  int8_t v5,  int8_t v6,  int8_t v7,
      int8_t v8,  int8_t v9,  int8_t v10, int8_t v11, int8_t v12, int8_t v13, int8_t v14, int8_t v15
    ) : simd8(int8x16_t{
      v0, v1, v2, v3, v4, v5, v6, v7,
      v8, v9, v10,v11,v12,v13,v14,v15
    }) {}
#endif
    // Repeat 16 values as many times as necessary (usually for lookup tables)
    simdjson_really_inline static simd8<int8_t> repeat_16(
      int8_t v0,  int8_t v1,  int8_t v2,  int8_t v3,  int8_t v4,  int8_t v5,  int8_t v6,  int8_t v7,
      int8_t v8,  int8_t v9,  int8_t v10, int8_t v11, int8_t v12, int8_t v13, int8_t v14, int8_t v15
    ) {
      return simd8<int8_t>(
        v0, v1, v2, v3, v4, v5, v6, v7,
        v8, v9, v10,v11,v12,v13,v14,v15
      );
    }

    // Store to array
    simdjson_really_inline void store(int8_t dst[16]) const { return vst1q_s8(dst, *this); }

    // Explicit conversion to/from unsigned
    //
    // Under Visual Studio/ARM64 uint8x16_t and int8x16_t are apparently the same type.
    // In theory, we could check this occurrence with std::same_as and std::enabled_if but it is C++14
    // and relatively ugly and hard to read.
#ifndef SIMDJSON_REGULAR_VISUAL_STUDIO
    simdjson_really_inline explicit simd8(const uint8x16_t other): simd8(vreinterpretq_s8_u8(other)) {}
#endif
    simdjson_really_inline explicit operator simd8<uint8_t>() const { return vreinterpretq_u8_s8(this->value); }

    // Math
    simdjson_really_inline simd8<int8_t> operator+(const simd8<int8_t> other) const { return vaddq_s8(*this, other); }
    simdjson_really_inline simd8<int8_t> operator-(const simd8<int8_t> other) const { return vsubq_s8(*this, other); }
    simdjson_really_inline simd8<int8_t>& operator+=(const simd8<int8_t> other) { *this = *this + other; return *this; }
    simdjson_really_inline simd8<int8_t>& operator-=(const simd8<int8_t> other) { *this = *this - other; return *this; }

    // Order-sensitive comparisons
    simdjson_really_inline simd8<int8_t> max_val(const simd8<int8_t> other) const { return vmaxq_s8(*this, other); }
    simdjson_really_inline simd8<int8_t> min_val(const simd8<int8_t> other) const { return vminq_s8(*this, other); }
    simdjson_really_inline simd8<bool> operator>(const simd8<int8_t> other) const { return vcgtq_s8(*this, other); }
    simdjson_really_inline simd8<bool> operator<(const simd8<int8_t> other) const { return vcltq_s8(*this, other); }
    simdjson_really_inline simd8<bool> operator==(const simd8<int8_t> other) const { return vceqq_s8(*this, other); }

    template<int N=1>
    simdjson_really_inline simd8<int8_t> prev(const simd8<int8_t> prev_chunk) const {
      return vextq_s8(prev_chunk, *this, 16 - N);
    }

    // Perform a lookup assuming no value is larger than 16
    template<typename L>
    simdjson_really_inline simd8<L> lookup_16(simd8<L> lookup_table) const {
      return lookup_table.apply_lookup_16_to(*this);
    }
    template<typename L>
    simdjson_really_inline simd8<L> lookup_16(
        L replace0,  L replace1,  L replace2,  L replace3,
        L replace4,  L replace5,  L replace6,  L replace7,
        L replace8,  L replace9,  L replace10, L replace11,
        L replace12, L replace13, L replace14, L replace15) const {
      return lookup_16(simd8<L>::repeat_16(
        replace0,  replace1,  replace2,  replace3,
        replace4,  replace5,  replace6,  replace7,
        replace8,  replace9,  replace10, replace11,
        replace12, replace13, replace14, replace15
      ));
    }

    template<typename T>
    simdjson_really_inline simd8<int8_t> apply_lookup_16_to(const simd8<T> original) {
      return vqtbl1q_s8(*this, simd8<uint8_t>(original));
    }
  };

  template<typename T>
  struct simd8x64 {
    static constexpr int NUM_CHUNKS = 64 / sizeof(simd8<T>);
    static_assert(NUM_CHUNKS == 4, "ARM kernel should use four registers per 64-byte block.");
    const simd8<T> chunks[NUM_CHUNKS];

    simd8x64(const simd8x64<T>& o) = delete; // no copy allowed
    simd8x64<T>& operator=(const simd8<T>& other) = delete; // no assignment allowed
    simd8x64() = delete; // no default constructor allowed

    simdjson_really_inline simd8x64(const simd8<T> chunk0, const simd8<T> chunk1, const simd8<T> chunk2, const simd8<T> chunk3) : chunks{chunk0, chunk1, chunk2, chunk3} {}
    simdjson_really_inline simd8x64(const T ptr[64]) : chunks{simd8<T>::load(ptr), simd8<T>::load(ptr+16), simd8<T>::load(ptr+32), simd8<T>::load(ptr+48)} {}

    simdjson_really_inline void store(T ptr[64]) const {
      this->chunks[0].store(ptr+sizeof(simd8<T>)*0);
      this->chunks[1].store(ptr+sizeof(simd8<T>)*1);
      this->chunks[2].store(ptr+sizeof(simd8<T>)*2);
      this->chunks[3].store(ptr+sizeof(simd8<T>)*3);
    }

    simdjson_really_inline simd8<T> reduce_or() const {
      return (this->chunks[0] | this->chunks[1]) | (this->chunks[2] | this->chunks[3]);
    }


    simdjson_really_inline void compress(uint64_t mask, T * output) const {
      this->chunks[0].compress(uint16_t(mask), output);
      this->chunks[1].compress(uint16_t(mask >> 16), output + 16 - count_ones(mask & 0xFFFF));
      this->chunks[2].compress(uint16_t(mask >> 32), output + 32 - count_ones(mask & 0xFFFFFFFF));
      this->chunks[3].compress(uint16_t(mask >> 48), output + 48 - count_ones(mask & 0xFFFFFFFFFFFF));
    }

    simdjson_really_inline uint64_t to_bitmask() const {
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
      const uint8x16_t bit_mask = make_uint8x16_t(
        0x01, 0x02, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80,
        0x01, 0x02, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80
      );
#else
      const uint8x16_t bit_mask = {
        0x01, 0x02, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80,
        0x01, 0x02, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80
      };
#endif
      // Add each of the elements next to each other, successively, to stuff each 8 byte mask into one.
      uint8x16_t sum0 = vpaddq_u8(this->chunks[0] & bit_mask, this->chunks[1] & bit_mask);
      uint8x16_t sum1 = vpaddq_u8(this->chunks[2] & bit_mask, this->chunks[3] & bit_mask);
      sum0 = vpaddq_u8(sum0, sum1);
      sum0 = vpaddq_u8(sum0, sum0);
      return vgetq_lane_u64(vreinterpretq_u64_u8(sum0), 0);
    }

    simdjson_really_inline uint64_t eq(const T m) const {
      const simd8<T> mask = simd8<T>::splat(m);
      return  simd8x64<bool>(
        this->chunks[0] == mask,
        this->chunks[1] == mask,
        this->chunks[2] == mask,
        this->chunks[3] == mask
      ).to_bitmask();
    }

    simdjson_really_inline uint64_t lteq(const T m) const {
      const simd8<T> mask = simd8<T>::splat(m);
      return  simd8x64<bool>(
        this->chunks[0] <= mask,
        this->chunks[1] <= mask,
        this->chunks[2] <= mask,
        this->chunks[3] <= mask
      ).to_bitmask();
    }
  }; // struct simd8x64<T>

} // namespace simd
} // unnamed namespace
} // namespace arm64
} // namespace simdjson

#endif // SIMDJSON_ARM64_SIMD_H
/* end file include/simdjson/arm64/simd.h */
/* begin file include/simdjson/arm64/end.h */
/* end file include/simdjson/arm64/end.h */

#endif // SIMDJSON_IMPLEMENTATION_ARM64

#endif // SIMDJSON_ARM64_H
/* end file include/simdjson/arm64.h */
/* begin file include/simdjson/fallback.h */
#ifndef SIMDJSON_FALLBACK_H
#define SIMDJSON_FALLBACK_H


#if SIMDJSON_IMPLEMENTATION_FALLBACK

namespace simdjson {
/**
 * Fallback implementation (runs on any machine).
 */
namespace fallback {
} // namespace fallback
} // namespace simdjson

/* begin file include/simdjson/fallback/implementation.h */
#ifndef SIMDJSON_FALLBACK_IMPLEMENTATION_H
#define SIMDJSON_FALLBACK_IMPLEMENTATION_H


namespace simdjson {
namespace fallback {

namespace {
using namespace simdjson;
}

class implementation final : public simdjson::implementation {
public:
  simdjson_really_inline implementation() : simdjson::implementation(
      "fallback",
      "Generic fallback implementation",
      0
  ) {}
  simdjson_warn_unused bool validate_utf8(const char *buf, size_t len) const noexcept final;
};

} // namespace fallback
} // namespace simdjson

#endif // SIMDJSON_FALLBACK_IMPLEMENTATION_H
/* end file include/simdjson/fallback/implementation.h */

/* begin file include/simdjson/fallback/begin.h */
// redefining SIMDJSON_IMPLEMENTATION to "fallback"
// #define SIMDJSON_IMPLEMENTATION fallback
/* end file include/simdjson/fallback/begin.h */

// Declarations
/* begin file include/simdjson/fallback/bitmanipulation.h */
#ifndef SIMDJSON_FALLBACK_BITMANIPULATION_H
#define SIMDJSON_FALLBACK_BITMANIPULATION_H

#include <limits>

namespace simdjson {
namespace fallback {
namespace {

#if defined(_MSC_VER) && !defined(_M_ARM64) && !defined(_M_X64)
static inline unsigned char _BitScanForward64(unsigned long* ret, uint64_t x) {
  unsigned long x0 = (unsigned long)x, top, bottom;
  _BitScanForward(&top, (unsigned long)(x >> 32));
  _BitScanForward(&bottom, x0);
  *ret = x0 ? bottom : 32 + top;
  return x != 0;
}
static unsigned char _BitScanReverse64(unsigned long* ret, uint64_t x) {
  unsigned long x1 = (unsigned long)(x >> 32), top, bottom;
  _BitScanReverse(&top, x1);
  _BitScanReverse(&bottom, (unsigned long)x);
  *ret = x1 ? top + 32 : bottom;
  return x != 0;
}
#endif

/* result might be undefined when input_num is zero */
simdjson_really_inline int leading_zeroes(uint64_t input_num) {
#ifdef _MSC_VER
  unsigned long leading_zero = 0;
  // Search the mask data from most significant bit (MSB)
  // to least significant bit (LSB) for a set bit (1).
  if (_BitScanReverse64(&leading_zero, input_num))
    return (int)(63 - leading_zero);
  else
    return 64;
#else
  return __builtin_clzll(input_num);
#endif// _MSC_VER
}

} // unnamed namespace
} // namespace fallback
} // namespace simdjson

#endif // SIMDJSON_FALLBACK_BITMANIPULATION_H
/* end file include/simdjson/fallback/bitmanipulation.h */
/* begin file include/simdjson/fallback/end.h */
/* end file include/simdjson/fallback/end.h */

#endif // SIMDJSON_IMPLEMENTATION_FALLBACK
#endif // SIMDJSON_FALLBACK_H
/* end file include/simdjson/fallback.h */
/* begin file include/simdjson/haswell.h */
#ifndef SIMDJSON_HASWELL_H
#define SIMDJSON_HASWELL_H


#if SIMDJSON_IMPLEMENTATION_HASWELL

#if SIMDJSON_CAN_ALWAYS_RUN_HASWELL
#define SIMDJSON_TARGET_HASWELL
#define SIMDJSON_UNTARGET_HASWELL
#else
#define SIMDJSON_TARGET_HASWELL SIMDJSON_TARGET_REGION("avx2,bmi,pclmul,lzcnt")
#define SIMDJSON_UNTARGET_HASWELL SIMDJSON_UNTARGET_REGION
#endif

namespace simdjson {
/**
 * Implementation for Haswell (Intel AVX2).
 */
namespace haswell {
} // namespace haswell
} // namespace simdjson

//
// These two need to be included outside SIMDJSON_TARGET_HASWELL
//
/* begin file include/simdjson/haswell/implementation.h */
#ifndef SIMDJSON_HASWELL_IMPLEMENTATION_H
#define SIMDJSON_HASWELL_IMPLEMENTATION_H


// The constructor may be executed on any host, so we take care not to use SIMDJSON_TARGET_HASWELL
namespace simdjson {
namespace haswell {

using namespace simdjson;

class implementation final : public simdjson::implementation {
public:
  simdjson_really_inline implementation() : simdjson::implementation(
      "haswell",
      "Intel/AMD AVX2",
      internal::instruction_set::AVX2 | internal::instruction_set::PCLMULQDQ | internal::instruction_set::BMI1 | internal::instruction_set::BMI2
  ) {}
  simdjson_warn_unused bool validate_utf8(const char *buf, size_t len) const noexcept final;
};

} // namespace haswell
} // namespace simdjson

#endif // SIMDJSON_HASWELL_IMPLEMENTATION_H
/* end file include/simdjson/haswell/implementation.h */
/* begin file include/simdjson/haswell/intrinsics.h */
#ifndef SIMDJSON_HASWELL_INTRINSICS_H
#define SIMDJSON_HASWELL_INTRINSICS_H


#ifdef SIMDJSON_VISUAL_STUDIO
// under clang within visual studio, this will include <x86intrin.h>
#include <intrin.h>  // visual studio or clang
#else
#include <x86intrin.h> // elsewhere
#endif // SIMDJSON_VISUAL_STUDIO

#ifdef SIMDJSON_CLANG_VISUAL_STUDIO
/**
 * You are not supposed, normally, to include these
 * headers directly. Instead you should either include intrin.h
 * or x86intrin.h. However, when compiling with clang
 * under Windows (i.e., when _MSC_VER is set), these headers
 * only get included *if* the corresponding features are detected
 * from macros:
 * e.g., if __AVX2__ is set... in turn,  we normally set these
 * macros by compiling against the corresponding architecture
 * (e.g., arch:AVX2, -mavx2, etc.) which compiles the whole
 * software with these advanced instructions. In simdjson, we
 * want to compile the whole program for a generic target,
 * and only target our specific kernels. As a workaround,
 * we directly include the needed headers. These headers would
 * normally guard against such usage, but we carefully included
 * <x86intrin.h>  (or <intrin.h>) before, so the headers
 * are fooled.
 */
#include <bmiintrin.h>   // for _blsr_u64
#include <lzcntintrin.h> // for  __lzcnt64
#include <immintrin.h>   // for most things (AVX2, AVX512, _popcnt64)
#include <smmintrin.h>
#include <tmmintrin.h>
#include <avxintrin.h>
#include <avx2intrin.h>
#include <wmmintrin.h>   // for  _mm_clmulepi64_si128
// unfortunately, we may not get _blsr_u64, but, thankfully, clang
// has it as a macro.
#ifndef _blsr_u64
// we roll our own
SIMDJSON_TARGET_HASWELL
static simdjson_really_inline uint64_t _blsr_u64(uint64_t n) {
  return (n - 1) & n;
}
SIMDJSON_UNTARGET_HASWELL
#endif //  _blsr_u64
#endif // SIMDJSON_CLANG_VISUAL_STUDIO

#endif // SIMDJSON_HASWELL_INTRINSICS_H
/* end file include/simdjson/haswell/intrinsics.h */

//
// The rest need to be inside the region
//
/* begin file include/simdjson/haswell/begin.h */
// redefining SIMDJSON_IMPLEMENTATION to "haswell"
// #define SIMDJSON_IMPLEMENTATION haswell
SIMDJSON_TARGET_HASWELL
/* end file include/simdjson/haswell/begin.h */

// Declarations
/* begin file include/simdjson/haswell/bitmanipulation.h */
#ifndef SIMDJSON_HASWELL_BITMANIPULATION_H
#define SIMDJSON_HASWELL_BITMANIPULATION_H

namespace simdjson {
namespace haswell {
namespace {

// We sometimes call trailing_zero on inputs that are zero,
// but the algorithms do not end up using the returned value.
// Sadly, sanitizers are not smart enough to figure it out.
NO_SANITIZE_UNDEFINED
simdjson_really_inline int trailing_zeroes(uint64_t input_num) {
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
  return (int)_tzcnt_u64(input_num);
#else // SIMDJSON_REGULAR_VISUAL_STUDIO
  ////////
  // You might expect the next line to be equivalent to
  // return (int)_tzcnt_u64(input_num);
  // but the generated code differs and might be less efficient?
  ////////
  return __builtin_ctzll(input_num);
#endif // SIMDJSON_REGULAR_VISUAL_STUDIO
}

/* result might be undefined when input_num is zero */
simdjson_really_inline uint64_t clear_lowest_bit(uint64_t input_num) {
  return _blsr_u64(input_num);
}

/* result might be undefined when input_num is zero */
simdjson_really_inline int leading_zeroes(uint64_t input_num) {
  return int(_lzcnt_u64(input_num));
}

#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
simdjson_really_inline unsigned __int64 count_ones(uint64_t input_num) {
  // note: we do not support legacy 32-bit Windows
  return __popcnt64(input_num);// Visual Studio wants two underscores
}
#else
simdjson_really_inline long long int count_ones(uint64_t input_num) {
  return _popcnt64(input_num);
}
#endif

simdjson_really_inline bool add_overflow(uint64_t value1, uint64_t value2,
                                uint64_t *result) {
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
  return _addcarry_u64(0, value1, value2,
                       reinterpret_cast<unsigned __int64 *>(result));
#else
  return __builtin_uaddll_overflow(value1, value2,
                                   reinterpret_cast<unsigned long long *>(result));
#endif
}

} // unnamed namespace
} // namespace haswell
} // namespace simdjson

#endif // SIMDJSON_HASWELL_BITMANIPULATION_H
/* end file include/simdjson/haswell/bitmanipulation.h */
/* begin file include/simdjson/haswell/simd.h */
#ifndef SIMDJSON_HASWELL_SIMD_H
#define SIMDJSON_HASWELL_SIMD_H


namespace simdjson {
namespace haswell {
namespace {
namespace simd {

  // Forward-declared so they can be used by splat and friends.
  template<typename Child>
  struct base {
    __m256i value;

    // Zero constructor
    simdjson_really_inline base() : value{__m256i()} {}

    // Conversion from SIMD register
    simdjson_really_inline base(const __m256i _value) : value(_value) {}

    // Conversion to SIMD register
    simdjson_really_inline operator const __m256i&() const { return this->value; }
    simdjson_really_inline operator __m256i&() { return this->value; }

    // Bit operations
    simdjson_really_inline Child operator|(const Child other) const { return _mm256_or_si256(*this, other); }
    simdjson_really_inline Child operator&(const Child other) const { return _mm256_and_si256(*this, other); }
    simdjson_really_inline Child operator^(const Child other) const { return _mm256_xor_si256(*this, other); }
    simdjson_really_inline Child bit_andnot(const Child other) const { return _mm256_andnot_si256(other, *this); }
    simdjson_really_inline Child& operator|=(const Child other) { auto this_cast = static_cast<Child*>(this); *this_cast = *this_cast | other; return *this_cast; }
    simdjson_really_inline Child& operator&=(const Child other) { auto this_cast = static_cast<Child*>(this); *this_cast = *this_cast & other; return *this_cast; }
    simdjson_really_inline Child& operator^=(const Child other) { auto this_cast = static_cast<Child*>(this); *this_cast = *this_cast ^ other; return *this_cast; }
  };

  // Forward-declared so they can be used by splat and friends.
  template<typename T>
  struct simd8;

  template<typename T, typename Mask=simd8<bool>>
  struct base8: base<simd8<T>> {
    typedef uint32_t bitmask_t;
    typedef uint64_t bitmask2_t;

    simdjson_really_inline base8() : base<simd8<T>>() {}
    simdjson_really_inline base8(const __m256i _value) : base<simd8<T>>(_value) {}

    simdjson_really_inline Mask operator==(const simd8<T> other) const { return _mm256_cmpeq_epi8(*this, other); }

    static const int SIZE = sizeof(base<T>::value);

    template<int N=1>
    simdjson_really_inline simd8<T> prev(const simd8<T> prev_chunk) const {
      return _mm256_alignr_epi8(*this, _mm256_permute2x128_si256(prev_chunk, *this, 0x21), 16 - N);
    }
  };

  // SIMD byte mask type (returned by things like eq and gt)
  template<>
  struct simd8<bool>: base8<bool> {
    static simdjson_really_inline simd8<bool> splat(bool _value) { return _mm256_set1_epi8(uint8_t(-(!!_value))); }

    simdjson_really_inline simd8<bool>() : base8() {}
    simdjson_really_inline simd8<bool>(const __m256i _value) : base8<bool>(_value) {}
    // Splat constructor
    simdjson_really_inline simd8<bool>(bool _value) : base8<bool>(splat(_value)) {}

    simdjson_really_inline int to_bitmask() const { return _mm256_movemask_epi8(*this); }
    simdjson_really_inline bool any() const { return !_mm256_testz_si256(*this, *this); }
    simdjson_really_inline simd8<bool> operator~() const { return *this ^ true; }
  };

  template<typename T>
  struct base8_numeric: base8<T> {
    static simdjson_really_inline simd8<T> splat(T _value) { return _mm256_set1_epi8(_value); }
    static simdjson_really_inline simd8<T> zero() { return _mm256_setzero_si256(); }
    static simdjson_really_inline simd8<T> load(const T values[32]) {
      return _mm256_loadu_si256(reinterpret_cast<const __m256i *>(values));
    }
    // Repeat 16 values as many times as necessary (usually for lookup tables)
    static simdjson_really_inline simd8<T> repeat_16(
      T v0,  T v1,  T v2,  T v3,  T v4,  T v5,  T v6,  T v7,
      T v8,  T v9,  T v10, T v11, T v12, T v13, T v14, T v15
    ) {
      return simd8<T>(
        v0, v1, v2, v3, v4, v5, v6, v7,
        v8, v9, v10,v11,v12,v13,v14,v15,
        v0, v1, v2, v3, v4, v5, v6, v7,
        v8, v9, v10,v11,v12,v13,v14,v15
      );
    }

    simdjson_really_inline base8_numeric() : base8<T>() {}
    simdjson_really_inline base8_numeric(const __m256i _value) : base8<T>(_value) {}

    // Store to array
    simdjson_really_inline void store(T dst[32]) const { return _mm256_storeu_si256(reinterpret_cast<__m256i *>(dst), *this); }

    // Addition/subtraction are the same for signed and unsigned
    simdjson_really_inline simd8<T> operator+(const simd8<T> other) const { return _mm256_add_epi8(*this, other); }
    simdjson_really_inline simd8<T> operator-(const simd8<T> other) const { return _mm256_sub_epi8(*this, other); }
    simdjson_really_inline simd8<T>& operator+=(const simd8<T> other) { *this = *this + other; return *static_cast<simd8<T>*>(this); }
    simdjson_really_inline simd8<T>& operator-=(const simd8<T> other) { *this = *this - other; return *static_cast<simd8<T>*>(this); }

    // Override to distinguish from bool version
    simdjson_really_inline simd8<T> operator~() const { return *this ^ 0xFFu; }

    // Perform a lookup assuming the value is between 0 and 16 (undefined behavior for out of range values)
    template<typename L>
    simdjson_really_inline simd8<L> lookup_16(simd8<L> lookup_table) const {
      return _mm256_shuffle_epi8(lookup_table, *this);
    }

    // Copies to 'output" all bytes corresponding to a 0 in the mask (interpreted as a bitset).
    // Passing a 0 value for mask would be equivalent to writing out every byte to output.
    // Only the first 32 - count_ones(mask) bytes of the result are significant but 32 bytes
    // get written.
    // Design consideration: it seems like a function with the
    // signature simd8<L> compress(uint32_t mask) would be
    // sensible, but the AVX ISA makes this kind of approach difficult.
    template<typename L>
    simdjson_really_inline void compress(uint32_t mask, L * output) const {
      using internal::thintable_epi8;
      using internal::BitsSetTable256mul2;
      using internal::pshufb_combine_table;
      // this particular implementation was inspired by work done by @animetosho
      // we do it in four steps, first 8 bytes and then second 8 bytes...
      uint8_t mask1 = uint8_t(mask); // least significant 8 bits
      uint8_t mask2 = uint8_t(mask >> 8); // second least significant 8 bits
      uint8_t mask3 = uint8_t(mask >> 16); // ...
      uint8_t mask4 = uint8_t(mask >> 24); // ...
      // next line just loads the 64-bit values thintable_epi8[mask1] and
      // thintable_epi8[mask2] into a 128-bit register, using only
      // two instructions on most compilers.
      __m256i shufmask =  _mm256_set_epi64x(thintable_epi8[mask4], thintable_epi8[mask3],
        thintable_epi8[mask2], thintable_epi8[mask1]);
      // we increment by 0x08 the second half of the mask and so forth
      shufmask =
      _mm256_add_epi8(shufmask, _mm256_set_epi32(0x18181818, 0x18181818,
         0x10101010, 0x10101010, 0x08080808, 0x08080808, 0, 0));
      // this is the version "nearly pruned"
      __m256i pruned = _mm256_shuffle_epi8(*this, shufmask);
      // we still need to put the  pieces back together.
      // we compute the popcount of the first words:
      int pop1 = BitsSetTable256mul2[mask1];
      int pop3 = BitsSetTable256mul2[mask3];

      // then load the corresponding mask
      // could be done with _mm256_loadu2_m128i but many standard libraries omit this intrinsic.
      __m256i v256 = _mm256_castsi128_si256(
        _mm_loadu_si128(reinterpret_cast<const __m128i *>(pshufb_combine_table + pop1 * 8)));
      __m256i compactmask = _mm256_insertf128_si256(v256,
         _mm_loadu_si128(reinterpret_cast<const __m128i *>(pshufb_combine_table + pop3 * 8)), 1);
      __m256i almostthere =  _mm256_shuffle_epi8(pruned, compactmask);
      // We just need to write out the result.
      // This is the tricky bit that is hard to do
      // if we want to return a SIMD register, since there
      // is no single-instruction approach to recombine
      // the two 128-bit lanes with an offset.
      __m128i v128;
      v128 = _mm256_castsi256_si128(almostthere);
      _mm_storeu_si128( reinterpret_cast<__m128i *>(output), v128);
      v128 = _mm256_extractf128_si256(almostthere, 1);
      _mm_storeu_si128( reinterpret_cast<__m128i *>(output + 16 - count_ones(mask & 0xFFFF)), v128);
    }

    template<typename L>
    simdjson_really_inline simd8<L> lookup_16(
        L replace0,  L replace1,  L replace2,  L replace3,
        L replace4,  L replace5,  L replace6,  L replace7,
        L replace8,  L replace9,  L replace10, L replace11,
        L replace12, L replace13, L replace14, L replace15) const {
      return lookup_16(simd8<L>::repeat_16(
        replace0,  replace1,  replace2,  replace3,
        replace4,  replace5,  replace6,  replace7,
        replace8,  replace9,  replace10, replace11,
        replace12, replace13, replace14, replace15
      ));
    }
  };

  // Signed bytes
  template<>
  struct simd8<int8_t> : base8_numeric<int8_t> {
    simdjson_really_inline simd8() : base8_numeric<int8_t>() {}
    simdjson_really_inline simd8(const __m256i _value) : base8_numeric<int8_t>(_value) {}
    // Splat constructor
    simdjson_really_inline simd8(int8_t _value) : simd8(splat(_value)) {}
    // Array constructor
    simdjson_really_inline simd8(const int8_t values[32]) : simd8(load(values)) {}
    // Member-by-member initialization
    simdjson_really_inline simd8(
      int8_t v0,  int8_t v1,  int8_t v2,  int8_t v3,  int8_t v4,  int8_t v5,  int8_t v6,  int8_t v7,
      int8_t v8,  int8_t v9,  int8_t v10, int8_t v11, int8_t v12, int8_t v13, int8_t v14, int8_t v15,
      int8_t v16, int8_t v17, int8_t v18, int8_t v19, int8_t v20, int8_t v21, int8_t v22, int8_t v23,
      int8_t v24, int8_t v25, int8_t v26, int8_t v27, int8_t v28, int8_t v29, int8_t v30, int8_t v31
    ) : simd8(_mm256_setr_epi8(
      v0, v1, v2, v3, v4, v5, v6, v7,
      v8, v9, v10,v11,v12,v13,v14,v15,
      v16,v17,v18,v19,v20,v21,v22,v23,
      v24,v25,v26,v27,v28,v29,v30,v31
    )) {}
    // Repeat 16 values as many times as necessary (usually for lookup tables)
    simdjson_really_inline static simd8<int8_t> repeat_16(
      int8_t v0,  int8_t v1,  int8_t v2,  int8_t v3,  int8_t v4,  int8_t v5,  int8_t v6,  int8_t v7,
      int8_t v8,  int8_t v9,  int8_t v10, int8_t v11, int8_t v12, int8_t v13, int8_t v14, int8_t v15
    ) {
      return simd8<int8_t>(
        v0, v1, v2, v3, v4, v5, v6, v7,
        v8, v9, v10,v11,v12,v13,v14,v15,
        v0, v1, v2, v3, v4, v5, v6, v7,
        v8, v9, v10,v11,v12,v13,v14,v15
      );
    }

    // Order-sensitive comparisons
    simdjson_really_inline simd8<int8_t> max_val(const simd8<int8_t> other) const { return _mm256_max_epi8(*this, other); }
    simdjson_really_inline simd8<int8_t> min_val(const simd8<int8_t> other) const { return _mm256_min_epi8(*this, other); }
    simdjson_really_inline simd8<bool> operator>(const simd8<int8_t> other) const { return _mm256_cmpgt_epi8(*this, other); }
    simdjson_really_inline simd8<bool> operator<(const simd8<int8_t> other) const { return _mm256_cmpgt_epi8(other, *this); }
  };

  // Unsigned bytes
  template<>
  struct simd8<uint8_t>: base8_numeric<uint8_t> {
    simdjson_really_inline simd8() : base8_numeric<uint8_t>() {}
    simdjson_really_inline simd8(const __m256i _value) : base8_numeric<uint8_t>(_value) {}
    // Splat constructor
    simdjson_really_inline simd8(uint8_t _value) : simd8(splat(_value)) {}
    // Array constructor
    simdjson_really_inline simd8(const uint8_t values[32]) : simd8(load(values)) {}
    // Member-by-member initialization
    simdjson_really_inline simd8(
      uint8_t v0,  uint8_t v1,  uint8_t v2,  uint8_t v3,  uint8_t v4,  uint8_t v5,  uint8_t v6,  uint8_t v7,
      uint8_t v8,  uint8_t v9,  uint8_t v10, uint8_t v11, uint8_t v12, uint8_t v13, uint8_t v14, uint8_t v15,
      uint8_t v16, uint8_t v17, uint8_t v18, uint8_t v19, uint8_t v20, uint8_t v21, uint8_t v22, uint8_t v23,
      uint8_t v24, uint8_t v25, uint8_t v26, uint8_t v27, uint8_t v28, uint8_t v29, uint8_t v30, uint8_t v31
    ) : simd8(_mm256_setr_epi8(
      v0, v1, v2, v3, v4, v5, v6, v7,
      v8, v9, v10,v11,v12,v13,v14,v15,
      v16,v17,v18,v19,v20,v21,v22,v23,
      v24,v25,v26,v27,v28,v29,v30,v31
    )) {}
    // Repeat 16 values as many times as necessary (usually for lookup tables)
    simdjson_really_inline static simd8<uint8_t> repeat_16(
      uint8_t v0,  uint8_t v1,  uint8_t v2,  uint8_t v3,  uint8_t v4,  uint8_t v5,  uint8_t v6,  uint8_t v7,
      uint8_t v8,  uint8_t v9,  uint8_t v10, uint8_t v11, uint8_t v12, uint8_t v13, uint8_t v14, uint8_t v15
    ) {
      return simd8<uint8_t>(
        v0, v1, v2, v3, v4, v5, v6, v7,
        v8, v9, v10,v11,v12,v13,v14,v15,
        v0, v1, v2, v3, v4, v5, v6, v7,
        v8, v9, v10,v11,v12,v13,v14,v15
      );
    }

    // Saturated math
    simdjson_really_inline simd8<uint8_t> saturating_add(const simd8<uint8_t> other) const { return _mm256_adds_epu8(*this, other); }
    simdjson_really_inline simd8<uint8_t> saturating_sub(const simd8<uint8_t> other) const { return _mm256_subs_epu8(*this, other); }

    // Order-specific operations
    simdjson_really_inline simd8<uint8_t> max_val(const simd8<uint8_t> other) const { return _mm256_max_epu8(*this, other); }
    simdjson_really_inline simd8<uint8_t> min_val(const simd8<uint8_t> other) const { return _mm256_min_epu8(other, *this); }
    // Same as >, but only guarantees true is nonzero (< guarantees true = -1)
    simdjson_really_inline simd8<uint8_t> gt_bits(const simd8<uint8_t> other) const { return this->saturating_sub(other); }
    // Same as <, but only guarantees true is nonzero (< guarantees true = -1)
    simdjson_really_inline simd8<uint8_t> lt_bits(const simd8<uint8_t> other) const { return other.saturating_sub(*this); }
    simdjson_really_inline simd8<bool> operator<=(const simd8<uint8_t> other) const { return other.max_val(*this) == other; }
    simdjson_really_inline simd8<bool> operator>=(const simd8<uint8_t> other) const { return other.min_val(*this) == other; }
    simdjson_really_inline simd8<bool> operator>(const simd8<uint8_t> other) const { return this->gt_bits(other).any_bits_set(); }
    simdjson_really_inline simd8<bool> operator<(const simd8<uint8_t> other) const { return this->lt_bits(other).any_bits_set(); }

    // Bit-specific operations
    simdjson_really_inline simd8<bool> bits_not_set() const { return *this == uint8_t(0); }
    simdjson_really_inline simd8<bool> bits_not_set(simd8<uint8_t> bits) const { return (*this & bits).bits_not_set(); }
    simdjson_really_inline simd8<bool> any_bits_set() const { return ~this->bits_not_set(); }
    simdjson_really_inline simd8<bool> any_bits_set(simd8<uint8_t> bits) const { return ~this->bits_not_set(bits); }
    simdjson_really_inline bool is_ascii() const { return _mm256_movemask_epi8(*this) == 0; }
    simdjson_really_inline bool bits_not_set_anywhere() const { return _mm256_testz_si256(*this, *this); }
    simdjson_really_inline bool any_bits_set_anywhere() const { return !bits_not_set_anywhere(); }
    simdjson_really_inline bool bits_not_set_anywhere(simd8<uint8_t> bits) const { return _mm256_testz_si256(*this, bits); }
    simdjson_really_inline bool any_bits_set_anywhere(simd8<uint8_t> bits) const { return !bits_not_set_anywhere(bits); }
    template<int N>
    simdjson_really_inline simd8<uint8_t> shr() const { return simd8<uint8_t>(_mm256_srli_epi16(*this, N)) & uint8_t(0xFFu >> N); }
    template<int N>
    simdjson_really_inline simd8<uint8_t> shl() const { return simd8<uint8_t>(_mm256_slli_epi16(*this, N)) & uint8_t(0xFFu << N); }
    // Get one of the bits and make a bitmask out of it.
    // e.g. value.get_bit<7>() gets the high bit
    template<int N>
    simdjson_really_inline int get_bit() const { return _mm256_movemask_epi8(_mm256_slli_epi16(*this, 7-N)); }
  };

  template<typename T>
  struct simd8x64 {
    static constexpr int NUM_CHUNKS = 64 / sizeof(simd8<T>);
    static_assert(NUM_CHUNKS == 2, "Haswell kernel should use two registers per 64-byte block.");
    const simd8<T> chunks[NUM_CHUNKS];

    simd8x64(const simd8x64<T>& o) = delete; // no copy allowed
    simd8x64<T>& operator=(const simd8<T>& other) = delete; // no assignment allowed
    simd8x64() = delete; // no default constructor allowed

    simdjson_really_inline simd8x64(const simd8<T> chunk0, const simd8<T> chunk1) : chunks{chunk0, chunk1} {}
    simdjson_really_inline simd8x64(const T ptr[64]) : chunks{simd8<T>::load(ptr), simd8<T>::load(ptr+32)} {}

    simdjson_really_inline void compress(uint64_t mask, T * output) const {
      uint32_t mask1 = uint32_t(mask);
      uint32_t mask2 = uint32_t(mask >> 32);
      this->chunks[0].compress(mask1, output);
      this->chunks[1].compress(mask2, output + 32 - count_ones(mask1));
    }

    simdjson_really_inline void store(T ptr[64]) const {
      this->chunks[0].store(ptr+sizeof(simd8<T>)*0);
      this->chunks[1].store(ptr+sizeof(simd8<T>)*1);
    }

    simdjson_really_inline uint64_t to_bitmask() const {
      uint64_t r_lo = uint32_t(this->chunks[0].to_bitmask());
      uint64_t r_hi =                       this->chunks[1].to_bitmask();
      return r_lo | (r_hi << 32);
    }

    simdjson_really_inline simd8<T> reduce_or() const {
      return this->chunks[0] | this->chunks[1];
    }

    simdjson_really_inline simd8x64<T> bit_or(const T m) const {
      const simd8<T> mask = simd8<T>::splat(m);
      return simd8x64<T>(
        this->chunks[0] | mask,
        this->chunks[1] | mask
      );
    }

    simdjson_really_inline uint64_t eq(const T m) const {
      const simd8<T> mask = simd8<T>::splat(m);
      return  simd8x64<bool>(
        this->chunks[0] == mask,
        this->chunks[1] == mask
      ).to_bitmask();
    }

    simdjson_really_inline uint64_t eq(const simd8x64<uint8_t> &other) const {
      return  simd8x64<bool>(
        this->chunks[0] == other.chunks[0],
        this->chunks[1] == other.chunks[1]
      ).to_bitmask();
    }

    simdjson_really_inline uint64_t lteq(const T m) const {
      const simd8<T> mask = simd8<T>::splat(m);
      return  simd8x64<bool>(
        this->chunks[0] <= mask,
        this->chunks[1] <= mask
      ).to_bitmask();
    }
  }; // struct simd8x64<T>

} // namespace simd

} // unnamed namespace
} // namespace haswell
} // namespace simdjson

#endif // SIMDJSON_HASWELL_SIMD_H
/* end file include/simdjson/haswell/simd.h */
/* begin file include/simdjson/haswell/end.h */
SIMDJSON_UNTARGET_HASWELL
/* end file include/simdjson/haswell/end.h */

#endif // SIMDJSON_IMPLEMENTATION_HASWELL
#endif // SIMDJSON_HASWELL_COMMON_H
/* end file include/simdjson/haswell.h */
/* begin file include/simdjson/ppc64.h */
#ifndef SIMDJSON_PPC64_H
#define SIMDJSON_PPC64_H


#if SIMDJSON_IMPLEMENTATION_PPC64

namespace simdjson {
/**
 * Implementation for ALTIVEC (PPC64).
 */
namespace ppc64 {
} // namespace ppc64
} // namespace simdjson

/* begin file include/simdjson/ppc64/implementation.h */
#ifndef SIMDJSON_PPC64_IMPLEMENTATION_H
#define SIMDJSON_PPC64_IMPLEMENTATION_H


namespace simdjson {
namespace ppc64 {

namespace {
using namespace simdjson;
using namespace simdjson::dom;
} // namespace

class implementation final : public simdjson::implementation {
public:
  simdjson_really_inline implementation()
      : simdjson::implementation("ppc64", "PPC64 ALTIVEC",
                                 internal::instruction_set::ALTIVEC) {}
  simdjson_warn_unused bool validate_utf8(const char *buf,
                                          size_t len) const noexcept final;
};

} // namespace ppc64
} // namespace simdjson

#endif // SIMDJSON_PPC64_IMPLEMENTATION_H
/* end file include/simdjson/ppc64/implementation.h */

/* begin file include/simdjson/ppc64/begin.h */
// redefining SIMDJSON_IMPLEMENTATION to "ppc64"
// #define SIMDJSON_IMPLEMENTATION ppc64
/* end file include/simdjson/ppc64/begin.h */

// Declarations
/* begin file include/simdjson/ppc64/intrinsics.h */
#ifndef SIMDJSON_PPC64_INTRINSICS_H
#define SIMDJSON_PPC64_INTRINSICS_H


// This should be the correct header whether
// you use visual studio or other compilers.
#include <altivec.h>

// These are defined by altivec.h in GCC toolchain, it is safe to undef them.
#ifdef bool
#undef bool
#endif

#ifdef vector
#undef vector
#endif

#endif //  SIMDJSON_PPC64_INTRINSICS_H
/* end file include/simdjson/ppc64/intrinsics.h */
/* begin file include/simdjson/ppc64/bitmanipulation.h */
#ifndef SIMDJSON_PPC64_BITMANIPULATION_H
#define SIMDJSON_PPC64_BITMANIPULATION_H

namespace simdjson {
namespace ppc64 {
namespace {

// We sometimes call trailing_zero on inputs that are zero,
// but the algorithms do not end up using the returned value.
// Sadly, sanitizers are not smart enough to figure it out.
NO_SANITIZE_UNDEFINED
simdjson_really_inline int trailing_zeroes(uint64_t input_num) {
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
  unsigned long ret;
  // Search the mask data from least significant bit (LSB)
  // to the most significant bit (MSB) for a set bit (1).
  _BitScanForward64(&ret, input_num);
  return (int)ret;
#else  // SIMDJSON_REGULAR_VISUAL_STUDIO
  return __builtin_ctzll(input_num);
#endif // SIMDJSON_REGULAR_VISUAL_STUDIO
}

/* result might be undefined when input_num is zero */
simdjson_really_inline uint64_t clear_lowest_bit(uint64_t input_num) {
  return input_num & (input_num - 1);
}

/* result might be undefined when input_num is zero */
simdjson_really_inline int leading_zeroes(uint64_t input_num) {
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
  unsigned long leading_zero = 0;
  // Search the mask data from most significant bit (MSB)
  // to least significant bit (LSB) for a set bit (1).
  if (_BitScanReverse64(&leading_zero, input_num))
    return (int)(63 - leading_zero);
  else
    return 64;
#else
  return __builtin_clzll(input_num);
#endif // SIMDJSON_REGULAR_VISUAL_STUDIO
}

#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
simdjson_really_inline int count_ones(uint64_t input_num) {
  // note: we do not support legacy 32-bit Windows
  return __popcnt64(input_num); // Visual Studio wants two underscores
}
#else
simdjson_really_inline int count_ones(uint64_t input_num) {
  return __builtin_popcountll(input_num);
}
#endif

simdjson_really_inline bool add_overflow(uint64_t value1, uint64_t value2,
                                         uint64_t *result) {
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
  *result = value1 + value2;
  return *result < value1;
#else
  return __builtin_uaddll_overflow(value1, value2,
                                   reinterpret_cast<unsigned long long *>(result));
#endif
}

} // unnamed namespace
} // namespace ppc64
} // namespace simdjson

#endif // SIMDJSON_PPC64_BITMANIPULATION_H
/* end file include/simdjson/ppc64/bitmanipulation.h */
/* begin file include/simdjson/ppc64/simd.h */
#ifndef SIMDJSON_PPC64_SIMD_H
#define SIMDJSON_PPC64_SIMD_H

#include <type_traits>

namespace simdjson {
namespace ppc64 {
namespace {
namespace simd {

using __m128i = __vector unsigned char;

template <typename Child> struct base {
  __m128i value;

  // Zero constructor
  simdjson_really_inline base() : value{__m128i()} {}

  // Conversion from SIMD register
  simdjson_really_inline base(const __m128i _value) : value(_value) {}

  // Conversion to SIMD register
  simdjson_really_inline operator const __m128i &() const {
    return this->value;
  }
  simdjson_really_inline operator __m128i &() { return this->value; }

  // Bit operations
  simdjson_really_inline Child operator|(const Child other) const {
    return vec_or(this->value, (__m128i)other);
  }
  simdjson_really_inline Child operator&(const Child other) const {
    return vec_and(this->value, (__m128i)other);
  }
  simdjson_really_inline Child operator^(const Child other) const {
    return vec_xor(this->value, (__m128i)other);
  }
  simdjson_really_inline Child bit_andnot(const Child other) const {
    return vec_andc(this->value, (__m128i)other);
  }
  simdjson_really_inline Child &operator|=(const Child other) {
    auto this_cast = static_cast<Child*>(this);
    *this_cast = *this_cast | other;
    return *this_cast;
  }
  simdjson_really_inline Child &operator&=(const Child other) {
    auto this_cast = static_cast<Child*>(this);
    *this_cast = *this_cast & other;
    return *this_cast;
  }
  simdjson_really_inline Child &operator^=(const Child other) {
    auto this_cast = static_cast<Child*>(this);
    *this_cast = *this_cast ^ other;
    return *this_cast;
  }
};

// Forward-declared so they can be used by splat and friends.
template <typename T> struct simd8;

template <typename T, typename Mask = simd8<bool>>
struct base8 : base<simd8<T>> {
  typedef uint16_t bitmask_t;
  typedef uint32_t bitmask2_t;

  simdjson_really_inline base8() : base<simd8<T>>() {}
  simdjson_really_inline base8(const __m128i _value) : base<simd8<T>>(_value) {}

  simdjson_really_inline Mask operator==(const simd8<T> other) const {
    return (__m128i)vec_cmpeq(this->value, (__m128i)other);
  }

  static const int SIZE = sizeof(base<simd8<T>>::value);

  template <int N = 1>
  simdjson_really_inline simd8<T> prev(simd8<T> prev_chunk) const {
    __m128i chunk = this->value;
#ifdef __LITTLE_ENDIAN__
    chunk = (__m128i)vec_reve(this->value);
    prev_chunk = (__m128i)vec_reve((__m128i)prev_chunk);
#endif
    chunk = (__m128i)vec_sld((__m128i)prev_chunk, (__m128i)chunk, 16 - N);
#ifdef __LITTLE_ENDIAN__
    chunk = (__m128i)vec_reve((__m128i)chunk);
#endif
    return chunk;
  }
};

// SIMD byte mask type (returned by things like eq and gt)
template <> struct simd8<bool> : base8<bool> {
  static simdjson_really_inline simd8<bool> splat(bool _value) {
    return (__m128i)vec_splats((unsigned char)(-(!!_value)));
  }

  simdjson_really_inline simd8<bool>() : base8() {}
  simdjson_really_inline simd8<bool>(const __m128i _value)
      : base8<bool>(_value) {}
  // Splat constructor
  simdjson_really_inline simd8<bool>(bool _value)
      : base8<bool>(splat(_value)) {}

  simdjson_really_inline int to_bitmask() const {
    __vector unsigned long long result;
    const __m128i perm_mask = {0x78, 0x70, 0x68, 0x60, 0x58, 0x50, 0x48, 0x40,
                               0x38, 0x30, 0x28, 0x20, 0x18, 0x10, 0x08, 0x00};

    result = ((__vector unsigned long long)vec_vbpermq((__m128i)this->value,
                                                       (__m128i)perm_mask));
#ifdef __LITTLE_ENDIAN__
    return static_cast<int>(result[1]);
#else
    return static_cast<int>(result[0]);
#endif
  }
  simdjson_really_inline bool any() const {
    return !vec_all_eq(this->value, (__m128i)vec_splats(0));
  }
  simdjson_really_inline simd8<bool> operator~() const {
    return this->value ^ (__m128i)splat(true);
  }
};

template <typename T> struct base8_numeric : base8<T> {
  static simdjson_really_inline simd8<T> splat(T value) {
    (void)value;
    return (__m128i)vec_splats(value);
  }
  static simdjson_really_inline simd8<T> zero() { return splat(0); }
  static simdjson_really_inline simd8<T> load(const T values[16]) {
    return (__m128i)(vec_vsx_ld(0, reinterpret_cast<const uint8_t *>(values)));
  }
  // Repeat 16 values as many times as necessary (usually for lookup tables)
  static simdjson_really_inline simd8<T> repeat_16(T v0, T v1, T v2, T v3, T v4,
                                                   T v5, T v6, T v7, T v8, T v9,
                                                   T v10, T v11, T v12, T v13,
                                                   T v14, T v15) {
    return simd8<T>(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13,
                    v14, v15);
  }

  simdjson_really_inline base8_numeric() : base8<T>() {}
  simdjson_really_inline base8_numeric(const __m128i _value)
      : base8<T>(_value) {}

  // Store to array
  simdjson_really_inline void store(T dst[16]) const {
    vec_vsx_st(this->value, 0, reinterpret_cast<__m128i *>(dst));
  }

  // Override to distinguish from bool version
  simdjson_really_inline simd8<T> operator~() const { return *this ^ 0xFFu; }

  // Addition/subtraction are the same for signed and unsigned
  simdjson_really_inline simd8<T> operator+(const simd8<T> other) const {
    return (__m128i)((__m128i)this->value + (__m128i)other);
  }
  simdjson_really_inline simd8<T> operator-(const simd8<T> other) const {
    return (__m128i)((__m128i)this->value - (__m128i)other);
  }
  simdjson_really_inline simd8<T> &operator+=(const simd8<T> other) {
    *this = *this + other;
    return *static_cast<simd8<T> *>(this);
  }
  simdjson_really_inline simd8<T> &operator-=(const simd8<T> other) {
    *this = *this - other;
    return *static_cast<simd8<T> *>(this);
  }

  // Perform a lookup assuming the value is between 0 and 16 (undefined behavior
  // for out of range values)
  template <typename L>
  simdjson_really_inline simd8<L> lookup_16(simd8<L> lookup_table) const {
    return (__m128i)vec_perm((__m128i)lookup_table, (__m128i)lookup_table, this->value);
  }

  // Copies to 'output" all bytes corresponding to a 0 in the mask (interpreted
  // as a bitset). Passing a 0 value for mask would be equivalent to writing out
  // every byte to output. Only the first 16 - count_ones(mask) bytes of the
  // result are significant but 16 bytes get written. Design consideration: it
  // seems like a function with the signature simd8<L> compress(uint32_t mask)
  // would be sensible, but the AVX ISA makes this kind of approach difficult.
  template <typename L>
  simdjson_really_inline void compress(uint16_t mask, L *output) const {
    using internal::BitsSetTable256mul2;
    using internal::pshufb_combine_table;
    using internal::thintable_epi8;
    // this particular implementation was inspired by work done by @animetosho
    // we do it in two steps, first 8 bytes and then second 8 bytes
    uint8_t mask1 = uint8_t(mask);      // least significant 8 bits
    uint8_t mask2 = uint8_t(mask >> 8); // most significant 8 bits
    // next line just loads the 64-bit values thintable_epi8[mask1] and
    // thintable_epi8[mask2] into a 128-bit register, using only
    // two instructions on most compilers.
#ifdef __LITTLE_ENDIAN__
    __m128i shufmask = (__m128i)(__vector unsigned long long){
        thintable_epi8[mask1], thintable_epi8[mask2]};
#else
    __m128i shufmask = (__m128i)(__vector unsigned long long){
        thintable_epi8[mask2], thintable_epi8[mask1]};
    shufmask = (__m128i)vec_reve((__m128i)shufmask);
#endif
    // we increment by 0x08 the second half of the mask
    shufmask = ((__m128i)shufmask) +
               ((__m128i)(__vector int){0, 0, 0x08080808, 0x08080808});

    // this is the version "nearly pruned"
    __m128i pruned = vec_perm(this->value, this->value, shufmask);
    // we still need to put the two halves together.
    // we compute the popcount of the first half:
    int pop1 = BitsSetTable256mul2[mask1];
    // then load the corresponding mask, what it does is to write
    // only the first pop1 bytes from the first 8 bytes, and then
    // it fills in with the bytes from the second 8 bytes + some filling
    // at the end.
    __m128i compactmask =
        vec_vsx_ld(0, reinterpret_cast<const uint8_t *>(pshufb_combine_table + pop1 * 8));
    __m128i answer = vec_perm(pruned, (__m128i)vec_splats(0), compactmask);
    vec_vsx_st(answer, 0, reinterpret_cast<__m128i *>(output));
  }

  template <typename L>
  simdjson_really_inline simd8<L>
  lookup_16(L replace0, L replace1, L replace2, L replace3, L replace4,
            L replace5, L replace6, L replace7, L replace8, L replace9,
            L replace10, L replace11, L replace12, L replace13, L replace14,
            L replace15) const {
    return lookup_16(simd8<L>::repeat_16(
        replace0, replace1, replace2, replace3, replace4, replace5, replace6,
        replace7, replace8, replace9, replace10, replace11, replace12,
        replace13, replace14, replace15));
  }
};

// Signed bytes
template <> struct simd8<int8_t> : base8_numeric<int8_t> {
  simdjson_really_inline simd8() : base8_numeric<int8_t>() {}
  simdjson_really_inline simd8(const __m128i _value)
      : base8_numeric<int8_t>(_value) {}
  // Splat constructor
  simdjson_really_inline simd8(int8_t _value) : simd8(splat(_value)) {}
  // Array constructor
  simdjson_really_inline simd8(const int8_t *values) : simd8(load(values)) {}
  // Member-by-member initialization
  simdjson_really_inline simd8(int8_t v0, int8_t v1, int8_t v2, int8_t v3,
                               int8_t v4, int8_t v5, int8_t v6, int8_t v7,
                               int8_t v8, int8_t v9, int8_t v10, int8_t v11,
                               int8_t v12, int8_t v13, int8_t v14, int8_t v15)
      : simd8((__m128i)(__vector signed char){v0, v1, v2, v3, v4, v5, v6, v7,
                                              v8, v9, v10, v11, v12, v13, v14,
                                              v15}) {}
  // Repeat 16 values as many times as necessary (usually for lookup tables)
  simdjson_really_inline static simd8<int8_t>
  repeat_16(int8_t v0, int8_t v1, int8_t v2, int8_t v3, int8_t v4, int8_t v5,
            int8_t v6, int8_t v7, int8_t v8, int8_t v9, int8_t v10, int8_t v11,
            int8_t v12, int8_t v13, int8_t v14, int8_t v15) {
    return simd8<int8_t>(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12,
                         v13, v14, v15);
  }

  // Order-sensitive comparisons
  simdjson_really_inline simd8<int8_t>
  max_val(const simd8<int8_t> other) const {
    return (__m128i)vec_max((__vector signed char)this->value,
                            (__vector signed char)(__m128i)other);
  }
  simdjson_really_inline simd8<int8_t>
  min_val(const simd8<int8_t> other) const {
    return (__m128i)vec_min((__vector signed char)this->value,
                            (__vector signed char)(__m128i)other);
  }
  simdjson_really_inline simd8<bool>
  operator>(const simd8<int8_t> other) const {
    return (__m128i)vec_cmpgt((__vector signed char)this->value,
                              (__vector signed char)(__m128i)other);
  }
  simdjson_really_inline simd8<bool>
  operator<(const simd8<int8_t> other) const {
    return (__m128i)vec_cmplt((__vector signed char)this->value,
                              (__vector signed char)(__m128i)other);
  }
};

// Unsigned bytes
template <> struct simd8<uint8_t> : base8_numeric<uint8_t> {
  simdjson_really_inline simd8() : base8_numeric<uint8_t>() {}
  simdjson_really_inline simd8(const __m128i _value)
      : base8_numeric<uint8_t>(_value) {}
  // Splat constructor
  simdjson_really_inline simd8(uint8_t _value) : simd8(splat(_value)) {}
  // Array constructor
  simdjson_really_inline simd8(const uint8_t *values) : simd8(load(values)) {}
  // Member-by-member initialization
  simdjson_really_inline
  simd8(uint8_t v0, uint8_t v1, uint8_t v2, uint8_t v3, uint8_t v4, uint8_t v5,
        uint8_t v6, uint8_t v7, uint8_t v8, uint8_t v9, uint8_t v10,
        uint8_t v11, uint8_t v12, uint8_t v13, uint8_t v14, uint8_t v15)
      : simd8((__m128i){v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12,
                        v13, v14, v15}) {}
  // Repeat 16 values as many times as necessary (usually for lookup tables)
  simdjson_really_inline static simd8<uint8_t>
  repeat_16(uint8_t v0, uint8_t v1, uint8_t v2, uint8_t v3, uint8_t v4,
            uint8_t v5, uint8_t v6, uint8_t v7, uint8_t v8, uint8_t v9,
            uint8_t v10, uint8_t v11, uint8_t v12, uint8_t v13, uint8_t v14,
            uint8_t v15) {
    return simd8<uint8_t>(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12,
                          v13, v14, v15);
  }

  // Saturated math
  simdjson_really_inline simd8<uint8_t>
  saturating_add(const simd8<uint8_t> other) const {
    return (__m128i)vec_adds(this->value, (__m128i)other);
  }
  simdjson_really_inline simd8<uint8_t>
  saturating_sub(const simd8<uint8_t> other) const {
    return (__m128i)vec_subs(this->value, (__m128i)other);
  }

  // Order-specific operations
  simdjson_really_inline simd8<uint8_t>
  max_val(const simd8<uint8_t> other) const {
    return (__m128i)vec_max(this->value, (__m128i)other);
  }
  simdjson_really_inline simd8<uint8_t>
  min_val(const simd8<uint8_t> other) const {
    return (__m128i)vec_min(this->value, (__m128i)other);
  }
  // Same as >, but only guarantees true is nonzero (< guarantees true = -1)
  simdjson_really_inline simd8<uint8_t>
  gt_bits(const simd8<uint8_t> other) const {
    return this->saturating_sub(other);
  }
  // Same as <, but only guarantees true is nonzero (< guarantees true = -1)
  simdjson_really_inline simd8<uint8_t>
  lt_bits(const simd8<uint8_t> other) const {
    return other.saturating_sub(*this);
  }
  simdjson_really_inline simd8<bool>
  operator<=(const simd8<uint8_t> other) const {
    return other.max_val(*this) == other;
  }
  simdjson_really_inline simd8<bool>
  operator>=(const simd8<uint8_t> other) const {
    return other.min_val(*this) == other;
  }
  simdjson_really_inline simd8<bool>
  operator>(const simd8<uint8_t> other) const {
    return this->gt_bits(other).any_bits_set();
  }
  simdjson_really_inline simd8<bool>
  operator<(const simd8<uint8_t> other) const {
    return this->gt_bits(other).any_bits_set();
  }

  // Bit-specific operations
  simdjson_really_inline simd8<bool> bits_not_set() const {
    return (__m128i)vec_cmpeq(this->value, (__m128i)vec_splats(uint8_t(0)));
  }
  simdjson_really_inline simd8<bool> bits_not_set(simd8<uint8_t> bits) const {
    return (*this & bits).bits_not_set();
  }
  simdjson_really_inline simd8<bool> any_bits_set() const {
    return ~this->bits_not_set();
  }
  simdjson_really_inline simd8<bool> any_bits_set(simd8<uint8_t> bits) const {
    return ~this->bits_not_set(bits);
  }
  simdjson_really_inline bool bits_not_set_anywhere() const {
    return vec_all_eq(this->value, (__m128i)vec_splats(0));
  }
  simdjson_really_inline bool any_bits_set_anywhere() const {
    return !bits_not_set_anywhere();
  }
  simdjson_really_inline bool bits_not_set_anywhere(simd8<uint8_t> bits) const {
    return vec_all_eq(vec_and(this->value, (__m128i)bits),
                      (__m128i)vec_splats(0));
  }
  simdjson_really_inline bool any_bits_set_anywhere(simd8<uint8_t> bits) const {
    return !bits_not_set_anywhere(bits);
  }
  template <int N> simdjson_really_inline simd8<uint8_t> shr() const {
    return simd8<uint8_t>(
        (__m128i)vec_sr(this->value, (__m128i)vec_splat_u8(N)));
  }
  template <int N> simdjson_really_inline simd8<uint8_t> shl() const {
    return simd8<uint8_t>(
        (__m128i)vec_sl(this->value, (__m128i)vec_splat_u8(N)));
  }
};

template <typename T> struct simd8x64 {
  static constexpr int NUM_CHUNKS = 64 / sizeof(simd8<T>);
  static_assert(NUM_CHUNKS == 4,
                "PPC64 kernel should use four registers per 64-byte block.");
  const simd8<T> chunks[NUM_CHUNKS];

  simd8x64(const simd8x64<T> &o) = delete; // no copy allowed
  simd8x64<T> &
  operator=(const simd8<T>& other) = delete; // no assignment allowed
  simd8x64() = delete;                      // no default constructor allowed

  simdjson_really_inline simd8x64(const simd8<T> chunk0, const simd8<T> chunk1,
                                  const simd8<T> chunk2, const simd8<T> chunk3)
      : chunks{chunk0, chunk1, chunk2, chunk3} {}
  simdjson_really_inline simd8x64(const T ptr[64])
      : chunks{simd8<T>::load(ptr), simd8<T>::load(ptr + 16),
               simd8<T>::load(ptr + 32), simd8<T>::load(ptr + 48)} {}

  simdjson_really_inline void store(T ptr[64]) const {
    this->chunks[0].store(ptr + sizeof(simd8<T>) * 0);
    this->chunks[1].store(ptr + sizeof(simd8<T>) * 1);
    this->chunks[2].store(ptr + sizeof(simd8<T>) * 2);
    this->chunks[3].store(ptr + sizeof(simd8<T>) * 3);
  }

  simdjson_really_inline simd8<T> reduce_or() const {
    return (this->chunks[0] | this->chunks[1]) |
           (this->chunks[2] | this->chunks[3]);
  }

  simdjson_really_inline void compress(uint64_t mask, T *output) const {
    this->chunks[0].compress(uint16_t(mask), output);
    this->chunks[1].compress(uint16_t(mask >> 16),
                             output + 16 - count_ones(mask & 0xFFFF));
    this->chunks[2].compress(uint16_t(mask >> 32),
                             output + 32 - count_ones(mask & 0xFFFFFFFF));
    this->chunks[3].compress(uint16_t(mask >> 48),
                             output + 48 - count_ones(mask & 0xFFFFFFFFFFFF));
  }

  simdjson_really_inline uint64_t to_bitmask() const {
    uint64_t r0 = uint32_t(this->chunks[0].to_bitmask());
    uint64_t r1 = this->chunks[1].to_bitmask();
    uint64_t r2 = this->chunks[2].to_bitmask();
    uint64_t r3 = this->chunks[3].to_bitmask();
    return r0 | (r1 << 16) | (r2 << 32) | (r3 << 48);
  }

  simdjson_really_inline uint64_t eq(const T m) const {
    const simd8<T> mask = simd8<T>::splat(m);
    return simd8x64<bool>(this->chunks[0] == mask, this->chunks[1] == mask,
                          this->chunks[2] == mask, this->chunks[3] == mask)
        .to_bitmask();
  }

  simdjson_really_inline uint64_t eq(const simd8x64<uint8_t> &other) const {
    return simd8x64<bool>(this->chunks[0] == other.chunks[0],
                          this->chunks[1] == other.chunks[1],
                          this->chunks[2] == other.chunks[2],
                          this->chunks[3] == other.chunks[3])
        .to_bitmask();
  }

  simdjson_really_inline uint64_t lteq(const T m) const {
    const simd8<T> mask = simd8<T>::splat(m);
    return simd8x64<bool>(this->chunks[0] <= mask, this->chunks[1] <= mask,
                          this->chunks[2] <= mask, this->chunks[3] <= mask)
        .to_bitmask();
  }
}; // struct simd8x64<T>

} // namespace simd
} // unnamed namespace
} // namespace ppc64
} // namespace simdjson

#endif // SIMDJSON_PPC64_SIMD_INPUT_H
/* end file include/simdjson/ppc64/simd.h */
/* begin file include/simdjson/ppc64/end.h */
/* end file include/simdjson/ppc64/end.h */

#endif // SIMDJSON_IMPLEMENTATION_PPC64

#endif // SIMDJSON_PPC64_H
/* end file include/simdjson/ppc64.h */
/* begin file include/simdjson/westmere.h */
#ifndef SIMDJSON_WESTMERE_H
#define SIMDJSON_WESTMERE_H


#if SIMDJSON_IMPLEMENTATION_WESTMERE

#if SIMDJSON_CAN_ALWAYS_RUN_WESTMERE
#define SIMDJSON_TARGET_WESTMERE
#define SIMDJSON_UNTARGET_WESTMERE
#else
#define SIMDJSON_TARGET_WESTMERE SIMDJSON_TARGET_REGION("sse4.2,pclmul")
#define SIMDJSON_UNTARGET_WESTMERE SIMDJSON_UNTARGET_REGION
#endif

namespace simdjson {
/**
 * Implementation for Westmere (Intel SSE4.2).
 */
namespace westmere {
} // namespace westmere
} // namespace simdjson

//
// These two need to be included outside SIMDJSON_TARGET_WESTMERE
//
/* begin file include/simdjson/westmere/implementation.h */
#ifndef SIMDJSON_WESTMERE_IMPLEMENTATION_H
#define SIMDJSON_WESTMERE_IMPLEMENTATION_H


// The constructor may be executed on any host, so we take care not to use SIMDJSON_TARGET_WESTMERE
namespace simdjson {
namespace westmere {

namespace {
using namespace simdjson;
}

class implementation final : public simdjson::implementation {
public:
  simdjson_really_inline implementation() : simdjson::implementation("westmere", "Intel/AMD SSE4.2", internal::instruction_set::SSE42 | internal::instruction_set::PCLMULQDQ) {}
  simdjson_warn_unused bool validate_utf8(const char *buf, size_t len) const noexcept final;
};

} // namespace westmere
} // namespace simdjson

#endif // SIMDJSON_WESTMERE_IMPLEMENTATION_H
/* end file include/simdjson/westmere/implementation.h */
/* begin file include/simdjson/westmere/intrinsics.h */
#ifndef SIMDJSON_WESTMERE_INTRINSICS_H
#define SIMDJSON_WESTMERE_INTRINSICS_H

#ifdef SIMDJSON_VISUAL_STUDIO
// under clang within visual studio, this will include <x86intrin.h>
#include <intrin.h> // visual studio or clang
#else
#include <x86intrin.h> // elsewhere
#endif // SIMDJSON_VISUAL_STUDIO


#ifdef SIMDJSON_CLANG_VISUAL_STUDIO
/**
 * You are not supposed, normally, to include these
 * headers directly. Instead you should either include intrin.h
 * or x86intrin.h. However, when compiling with clang
 * under Windows (i.e., when _MSC_VER is set), these headers
 * only get included *if* the corresponding features are detected
 * from macros:
 */
#include <smmintrin.h>  // for _mm_alignr_epi8
#include <wmmintrin.h>  // for  _mm_clmulepi64_si128
#endif



#endif // SIMDJSON_WESTMERE_INTRINSICS_H
/* end file include/simdjson/westmere/intrinsics.h */

//
// The rest need to be inside the region
//
/* begin file include/simdjson/westmere/begin.h */
// redefining SIMDJSON_IMPLEMENTATION to "westmere"
// #define SIMDJSON_IMPLEMENTATION westmere
SIMDJSON_TARGET_WESTMERE
/* end file include/simdjson/westmere/begin.h */

// Declarations
/* begin file include/simdjson/westmere/bitmanipulation.h */
#ifndef SIMDJSON_WESTMERE_BITMANIPULATION_H
#define SIMDJSON_WESTMERE_BITMANIPULATION_H

namespace simdjson {
namespace westmere {
namespace {

// We sometimes call trailing_zero on inputs that are zero,
// but the algorithms do not end up using the returned value.
// Sadly, sanitizers are not smart enough to figure it out.
NO_SANITIZE_UNDEFINED
simdjson_really_inline int trailing_zeroes(uint64_t input_num) {
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
  unsigned long ret;
  // Search the mask data from least significant bit (LSB)
  // to the most significant bit (MSB) for a set bit (1).
  _BitScanForward64(&ret, input_num);
  return (int)ret;
#else // SIMDJSON_REGULAR_VISUAL_STUDIO
  return __builtin_ctzll(input_num);
#endif // SIMDJSON_REGULAR_VISUAL_STUDIO
}

/* result might be undefined when input_num is zero */
simdjson_really_inline uint64_t clear_lowest_bit(uint64_t input_num) {
  return input_num & (input_num-1);
}

/* result might be undefined when input_num is zero */
simdjson_really_inline int leading_zeroes(uint64_t input_num) {
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
  unsigned long leading_zero = 0;
  // Search the mask data from most significant bit (MSB)
  // to least significant bit (LSB) for a set bit (1).
  if (_BitScanReverse64(&leading_zero, input_num))
    return (int)(63 - leading_zero);
  else
    return 64;
#else
  return __builtin_clzll(input_num);
#endif// SIMDJSON_REGULAR_VISUAL_STUDIO
}

#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
simdjson_really_inline unsigned __int64 count_ones(uint64_t input_num) {
  // note: we do not support legacy 32-bit Windows
  return __popcnt64(input_num);// Visual Studio wants two underscores
}
#else
simdjson_really_inline long long int count_ones(uint64_t input_num) {
  return _popcnt64(input_num);
}
#endif

simdjson_really_inline bool add_overflow(uint64_t value1, uint64_t value2,
                                uint64_t *result) {
#ifdef SIMDJSON_REGULAR_VISUAL_STUDIO
  return _addcarry_u64(0, value1, value2,
                       reinterpret_cast<unsigned __int64 *>(result));
#else
  return __builtin_uaddll_overflow(value1, value2,
                                   reinterpret_cast<unsigned long long *>(result));
#endif
}

} // unnamed namespace
} // namespace westmere
} // namespace simdjson

#endif // SIMDJSON_WESTMERE_BITMANIPULATION_H
/* end file include/simdjson/westmere/bitmanipulation.h */
/* begin file include/simdjson/westmere/simd.h */
#ifndef SIMDJSON_WESTMERE_SIMD_H
#define SIMDJSON_WESTMERE_SIMD_H


namespace simdjson {
namespace westmere {
namespace {
namespace simd {

  template<typename Child>
  struct base {
    __m128i value;

    // Zero constructor
    simdjson_really_inline base() : value{__m128i()} {}

    // Conversion from SIMD register
    simdjson_really_inline base(const __m128i _value) : value(_value) {}

    // Conversion to SIMD register
    simdjson_really_inline operator const __m128i&() const { return this->value; }
    simdjson_really_inline operator __m128i&() { return this->value; }

    // Bit operations
    simdjson_really_inline Child operator|(const Child other) const { return _mm_or_si128(*this, other); }
    simdjson_really_inline Child operator&(const Child other) const { return _mm_and_si128(*this, other); }
    simdjson_really_inline Child operator^(const Child other) const { return _mm_xor_si128(*this, other); }
    simdjson_really_inline Child bit_andnot(const Child other) const { return _mm_andnot_si128(other, *this); }
    simdjson_really_inline Child& operator|=(const Child other) { auto this_cast = static_cast<Child*>(this); *this_cast = *this_cast | other; return *this_cast; }
    simdjson_really_inline Child& operator&=(const Child other) { auto this_cast = static_cast<Child*>(this); *this_cast = *this_cast & other; return *this_cast; }
    simdjson_really_inline Child& operator^=(const Child other) { auto this_cast = static_cast<Child*>(this); *this_cast = *this_cast ^ other; return *this_cast; }
  };

  // Forward-declared so they can be used by splat and friends.
  template<typename T>
  struct simd8;

  template<typename T, typename Mask=simd8<bool>>
  struct base8: base<simd8<T>> {
    typedef uint16_t bitmask_t;
    typedef uint32_t bitmask2_t;

    simdjson_really_inline base8() : base<simd8<T>>() {}
    simdjson_really_inline base8(const __m128i _value) : base<simd8<T>>(_value) {}

    simdjson_really_inline Mask operator==(const simd8<T> other) const { return _mm_cmpeq_epi8(*this, other); }

    static const int SIZE = sizeof(base<simd8<T>>::value);

    template<int N=1>
    simdjson_really_inline simd8<T> prev(const simd8<T> prev_chunk) const {
      return _mm_alignr_epi8(*this, prev_chunk, 16 - N);
    }
  };

  // SIMD byte mask type (returned by things like eq and gt)
  template<>
  struct simd8<bool>: base8<bool> {
    static simdjson_really_inline simd8<bool> splat(bool _value) { return _mm_set1_epi8(uint8_t(-(!!_value))); }

    simdjson_really_inline simd8<bool>() : base8() {}
    simdjson_really_inline simd8<bool>(const __m128i _value) : base8<bool>(_value) {}
    // Splat constructor
    simdjson_really_inline simd8<bool>(bool _value) : base8<bool>(splat(_value)) {}

    simdjson_really_inline int to_bitmask() const { return _mm_movemask_epi8(*this); }
    simdjson_really_inline bool any() const { return !_mm_testz_si128(*this, *this); }
    simdjson_really_inline simd8<bool> operator~() const { return *this ^ true; }
  };

  template<typename T>
  struct base8_numeric: base8<T> {
    static simdjson_really_inline simd8<T> splat(T _value) { return _mm_set1_epi8(_value); }
    static simdjson_really_inline simd8<T> zero() { return _mm_setzero_si128(); }
    static simdjson_really_inline simd8<T> load(const T values[16]) {
      return _mm_loadu_si128(reinterpret_cast<const __m128i *>(values));
    }
    // Repeat 16 values as many times as necessary (usually for lookup tables)
    static simdjson_really_inline simd8<T> repeat_16(
      T v0,  T v1,  T v2,  T v3,  T v4,  T v5,  T v6,  T v7,
      T v8,  T v9,  T v10, T v11, T v12, T v13, T v14, T v15
    ) {
      return simd8<T>(
        v0, v1, v2, v3, v4, v5, v6, v7,
        v8, v9, v10,v11,v12,v13,v14,v15
      );
    }

    simdjson_really_inline base8_numeric() : base8<T>() {}
    simdjson_really_inline base8_numeric(const __m128i _value) : base8<T>(_value) {}

    // Store to array
    simdjson_really_inline void store(T dst[16]) const { return _mm_storeu_si128(reinterpret_cast<__m128i *>(dst), *this); }

    // Override to distinguish from bool version
    simdjson_really_inline simd8<T> operator~() const { return *this ^ 0xFFu; }

    // Addition/subtraction are the same for signed and unsigned
    simdjson_really_inline simd8<T> operator+(const simd8<T> other) const { return _mm_add_epi8(*this, other); }
    simdjson_really_inline simd8<T> operator-(const simd8<T> other) const { return _mm_sub_epi8(*this, other); }
    simdjson_really_inline simd8<T>& operator+=(const simd8<T> other) { *this = *this + other; return *static_cast<simd8<T>*>(this); }
    simdjson_really_inline simd8<T>& operator-=(const simd8<T> other) { *this = *this - other; return *static_cast<simd8<T>*>(this); }

    // Perform a lookup assuming the value is between 0 and 16 (undefined behavior for out of range values)
    template<typename L>
    simdjson_really_inline simd8<L> lookup_16(simd8<L> lookup_table) const {
      return _mm_shuffle_epi8(lookup_table, *this);
    }

    // Copies to 'output" all bytes corresponding to a 0 in the mask (interpreted as a bitset).
    // Passing a 0 value for mask would be equivalent to writing out every byte to output.
    // Only the first 16 - count_ones(mask) bytes of the result are significant but 16 bytes
    // get written.
    // Design consideration: it seems like a function with the
    // signature simd8<L> compress(uint32_t mask) would be
    // sensible, but the AVX ISA makes this kind of approach difficult.
    template<typename L>
    simdjson_really_inline void compress(uint16_t mask, L * output) const {
      using internal::thintable_epi8;
      using internal::BitsSetTable256mul2;
      using internal::pshufb_combine_table;
      // this particular implementation was inspired by work done by @animetosho
      // we do it in two steps, first 8 bytes and then second 8 bytes
      uint8_t mask1 = uint8_t(mask); // least significant 8 bits
      uint8_t mask2 = uint8_t(mask >> 8); // most significant 8 bits
      // next line just loads the 64-bit values thintable_epi8[mask1] and
      // thintable_epi8[mask2] into a 128-bit register, using only
      // two instructions on most compilers.
      __m128i shufmask =  _mm_set_epi64x(thintable_epi8[mask2], thintable_epi8[mask1]);
      // we increment by 0x08 the second half of the mask
      shufmask =
      _mm_add_epi8(shufmask, _mm_set_epi32(0x08080808, 0x08080808, 0, 0));
      // this is the version "nearly pruned"
      __m128i pruned = _mm_shuffle_epi8(*this, shufmask);
      // we still need to put the two halves together.
      // we compute the popcount of the first half:
      int pop1 = BitsSetTable256mul2[mask1];
      // then load the corresponding mask, what it does is to write
      // only the first pop1 bytes from the first 8 bytes, and then
      // it fills in with the bytes from the second 8 bytes + some filling
      // at the end.
      __m128i compactmask =
      _mm_loadu_si128(reinterpret_cast<const __m128i *>(pshufb_combine_table + pop1 * 8));
      __m128i answer = _mm_shuffle_epi8(pruned, compactmask);
      _mm_storeu_si128(reinterpret_cast<__m128i *>(output), answer);
    }

    template<typename L>
    simdjson_really_inline simd8<L> lookup_16(
        L replace0,  L replace1,  L replace2,  L replace3,
        L replace4,  L replace5,  L replace6,  L replace7,
        L replace8,  L replace9,  L replace10, L replace11,
        L replace12, L replace13, L replace14, L replace15) const {
      return lookup_16(simd8<L>::repeat_16(
        replace0,  replace1,  replace2,  replace3,
        replace4,  replace5,  replace6,  replace7,
        replace8,  replace9,  replace10, replace11,
        replace12, replace13, replace14, replace15
      ));
    }
  };

  // Signed bytes
  template<>
  struct simd8<int8_t> : base8_numeric<int8_t> {
    simdjson_really_inline simd8() : base8_numeric<int8_t>() {}
    simdjson_really_inline simd8(const __m128i _value) : base8_numeric<int8_t>(_value) {}
    // Splat constructor
    simdjson_really_inline simd8(int8_t _value) : simd8(splat(_value)) {}
    // Array constructor
    simdjson_really_inline simd8(const int8_t* values) : simd8(load(values)) {}
    // Member-by-member initialization
    simdjson_really_inline simd8(
      int8_t v0,  int8_t v1,  int8_t v2,  int8_t v3,  int8_t v4,  int8_t v5,  int8_t v6,  int8_t v7,
      int8_t v8,  int8_t v9,  int8_t v10, int8_t v11, int8_t v12, int8_t v13, int8_t v14, int8_t v15
    ) : simd8(_mm_setr_epi8(
      v0, v1, v2, v3, v4, v5, v6, v7,
      v8, v9, v10,v11,v12,v13,v14,v15
    )) {}
    // Repeat 16 values as many times as necessary (usually for lookup tables)
    simdjson_really_inline static simd8<int8_t> repeat_16(
      int8_t v0,  int8_t v1,  int8_t v2,  int8_t v3,  int8_t v4,  int8_t v5,  int8_t v6,  int8_t v7,
      int8_t v8,  int8_t v9,  int8_t v10, int8_t v11, int8_t v12, int8_t v13, int8_t v14, int8_t v15
    ) {
      return simd8<int8_t>(
        v0, v1, v2, v3, v4, v5, v6, v7,
        v8, v9, v10,v11,v12,v13,v14,v15
      );
    }

    // Order-sensitive comparisons
    simdjson_really_inline simd8<int8_t> max_val(const simd8<int8_t> other) const { return _mm_max_epi8(*this, other); }
    simdjson_really_inline simd8<int8_t> min_val(const simd8<int8_t> other) const { return _mm_min_epi8(*this, other); }
    simdjson_really_inline simd8<bool> operator>(const simd8<int8_t> other) const { return _mm_cmpgt_epi8(*this, other); }
    simdjson_really_inline simd8<bool> operator<(const simd8<int8_t> other) const { return _mm_cmpgt_epi8(other, *this); }
  };

  // Unsigned bytes
  template<>
  struct simd8<uint8_t>: base8_numeric<uint8_t> {
    simdjson_really_inline simd8() : base8_numeric<uint8_t>() {}
    simdjson_really_inline simd8(const __m128i _value) : base8_numeric<uint8_t>(_value) {}
    // Splat constructor
    simdjson_really_inline simd8(uint8_t _value) : simd8(splat(_value)) {}
    // Array constructor
    simdjson_really_inline simd8(const uint8_t* values) : simd8(load(values)) {}
    // Member-by-member initialization
    simdjson_really_inline simd8(
      uint8_t v0,  uint8_t v1,  uint8_t v2,  uint8_t v3,  uint8_t v4,  uint8_t v5,  uint8_t v6,  uint8_t v7,
      uint8_t v8,  uint8_t v9,  uint8_t v10, uint8_t v11, uint8_t v12, uint8_t v13, uint8_t v14, uint8_t v15
    ) : simd8(_mm_setr_epi8(
      v0, v1, v2, v3, v4, v5, v6, v7,
      v8, v9, v10,v11,v12,v13,v14,v15
    )) {}
    // Repeat 16 values as many times as necessary (usually for lookup tables)
    simdjson_really_inline static simd8<uint8_t> repeat_16(
      uint8_t v0,  uint8_t v1,  uint8_t v2,  uint8_t v3,  uint8_t v4,  uint8_t v5,  uint8_t v6,  uint8_t v7,
      uint8_t v8,  uint8_t v9,  uint8_t v10, uint8_t v11, uint8_t v12, uint8_t v13, uint8_t v14, uint8_t v15
    ) {
      return simd8<uint8_t>(
        v0, v1, v2, v3, v4, v5, v6, v7,
        v8, v9, v10,v11,v12,v13,v14,v15
      );
    }

    // Saturated math
    simdjson_really_inline simd8<uint8_t> saturating_add(const simd8<uint8_t> other) const { return _mm_adds_epu8(*this, other); }
    simdjson_really_inline simd8<uint8_t> saturating_sub(const simd8<uint8_t> other) const { return _mm_subs_epu8(*this, other); }

    // Order-specific operations
    simdjson_really_inline simd8<uint8_t> max_val(const simd8<uint8_t> other) const { return _mm_max_epu8(*this, other); }
    simdjson_really_inline simd8<uint8_t> min_val(const simd8<uint8_t> other) const { return _mm_min_epu8(*this, other); }
    // Same as >, but only guarantees true is nonzero (< guarantees true = -1)
    simdjson_really_inline simd8<uint8_t> gt_bits(const simd8<uint8_t> other) const { return this->saturating_sub(other); }
    // Same as <, but only guarantees true is nonzero (< guarantees true = -1)
    simdjson_really_inline simd8<uint8_t> lt_bits(const simd8<uint8_t> other) const { return other.saturating_sub(*this); }
    simdjson_really_inline simd8<bool> operator<=(const simd8<uint8_t> other) const { return other.max_val(*this) == other; }
    simdjson_really_inline simd8<bool> operator>=(const simd8<uint8_t> other) const { return other.min_val(*this) == other; }
    simdjson_really_inline simd8<bool> operator>(const simd8<uint8_t> other) const { return this->gt_bits(other).any_bits_set(); }
    simdjson_really_inline simd8<bool> operator<(const simd8<uint8_t> other) const { return this->gt_bits(other).any_bits_set(); }

    // Bit-specific operations
    simdjson_really_inline simd8<bool> bits_not_set() const { return *this == uint8_t(0); }
    simdjson_really_inline simd8<bool> bits_not_set(simd8<uint8_t> bits) const { return (*this & bits).bits_not_set(); }
    simdjson_really_inline simd8<bool> any_bits_set() const { return ~this->bits_not_set(); }
    simdjson_really_inline simd8<bool> any_bits_set(simd8<uint8_t> bits) const { return ~this->bits_not_set(bits); }
    simdjson_really_inline bool is_ascii() const { return _mm_movemask_epi8(*this) == 0; }
    simdjson_really_inline bool bits_not_set_anywhere() const { return _mm_testz_si128(*this, *this); }
    simdjson_really_inline bool any_bits_set_anywhere() const { return !bits_not_set_anywhere(); }
    simdjson_really_inline bool bits_not_set_anywhere(simd8<uint8_t> bits) const { return _mm_testz_si128(*this, bits); }
    simdjson_really_inline bool any_bits_set_anywhere(simd8<uint8_t> bits) const { return !bits_not_set_anywhere(bits); }
    template<int N>
    simdjson_really_inline simd8<uint8_t> shr() const { return simd8<uint8_t>(_mm_srli_epi16(*this, N)) & uint8_t(0xFFu >> N); }
    template<int N>
    simdjson_really_inline simd8<uint8_t> shl() const { return simd8<uint8_t>(_mm_slli_epi16(*this, N)) & uint8_t(0xFFu << N); }
    // Get one of the bits and make a bitmask out of it.
    // e.g. value.get_bit<7>() gets the high bit
    template<int N>
    simdjson_really_inline int get_bit() const { return _mm_movemask_epi8(_mm_slli_epi16(*this, 7-N)); }
  };

  template<typename T>
  struct simd8x64 {
    static constexpr int NUM_CHUNKS = 64 / sizeof(simd8<T>);
    static_assert(NUM_CHUNKS == 4, "Westmere kernel should use four registers per 64-byte block.");
    const simd8<T> chunks[NUM_CHUNKS];

    simd8x64(const simd8x64<T>& o) = delete; // no copy allowed
    simd8x64<T>& operator=(const simd8<T>& other) = delete; // no assignment allowed
    simd8x64() = delete; // no default constructor allowed

    simdjson_really_inline simd8x64(const simd8<T> chunk0, const simd8<T> chunk1, const simd8<T> chunk2, const simd8<T> chunk3) : chunks{chunk0, chunk1, chunk2, chunk3} {}
    simdjson_really_inline simd8x64(const T ptr[64]) : chunks{simd8<T>::load(ptr), simd8<T>::load(ptr+16), simd8<T>::load(ptr+32), simd8<T>::load(ptr+48)} {}

    simdjson_really_inline void store(T ptr[64]) const {
      this->chunks[0].store(ptr+sizeof(simd8<T>)*0);
      this->chunks[1].store(ptr+sizeof(simd8<T>)*1);
      this->chunks[2].store(ptr+sizeof(simd8<T>)*2);
      this->chunks[3].store(ptr+sizeof(simd8<T>)*3);
    }

    simdjson_really_inline simd8<T> reduce_or() const {
      return (this->chunks[0] | this->chunks[1]) | (this->chunks[2] | this->chunks[3]);
    }

    simdjson_really_inline void compress(uint64_t mask, T * output) const {
      this->chunks[0].compress(uint16_t(mask), output);
      this->chunks[1].compress(uint16_t(mask >> 16), output + 16 - count_ones(mask & 0xFFFF));
      this->chunks[2].compress(uint16_t(mask >> 32), output + 32 - count_ones(mask & 0xFFFFFFFF));
      this->chunks[3].compress(uint16_t(mask >> 48), output + 48 - count_ones(mask & 0xFFFFFFFFFFFF));
    }

    simdjson_really_inline uint64_t to_bitmask() const {
      uint64_t r0 = uint32_t(this->chunks[0].to_bitmask() );
      uint64_t r1 =          this->chunks[1].to_bitmask() ;
      uint64_t r2 =          this->chunks[2].to_bitmask() ;
      uint64_t r3 =          this->chunks[3].to_bitmask() ;
      return r0 | (r1 << 16) | (r2 << 32) | (r3 << 48);
    }

    simdjson_really_inline uint64_t eq(const T m) const {
      const simd8<T> mask = simd8<T>::splat(m);
      return  simd8x64<bool>(
        this->chunks[0] == mask,
        this->chunks[1] == mask,
        this->chunks[2] == mask,
        this->chunks[3] == mask
      ).to_bitmask();
    }

    simdjson_really_inline uint64_t eq(const simd8x64<uint8_t> &other) const {
      return  simd8x64<bool>(
        this->chunks[0] == other.chunks[0],
        this->chunks[1] == other.chunks[1],
        this->chunks[2] == other.chunks[2],
        this->chunks[3] == other.chunks[3]
      ).to_bitmask();
    }

    simdjson_really_inline uint64_t lteq(const T m) const {
      const simd8<T> mask = simd8<T>::splat(m);
      return  simd8x64<bool>(
        this->chunks[0] <= mask,
        this->chunks[1] <= mask,
        this->chunks[2] <= mask,
        this->chunks[3] <= mask
      ).to_bitmask();
    }
  }; // struct simd8x64<T>

} // namespace simd
} // unnamed namespace
} // namespace westmere
} // namespace simdjson

#endif // SIMDJSON_WESTMERE_SIMD_INPUT_H
/* end file include/simdjson/westmere/simd.h */
/* begin file include/simdjson/westmere/end.h */
SIMDJSON_UNTARGET_WESTMERE
/* end file include/simdjson/westmere/end.h */

#endif // SIMDJSON_IMPLEMENTATION_WESTMERE
#endif // SIMDJSON_WESTMERE_COMMON_H
/* end file include/simdjson/westmere.h */

// Builtin implementation

SIMDJSON_POP_DISABLE_WARNINGS

#endif // SIMDJSON_IMPLEMENTATIONS_H
/* end file include/simdjson/implementations.h */

// Determine the best builtin implementation
#ifndef SIMDJSON_BUILTIN_IMPLEMENTATION
#if SIMDJSON_CAN_ALWAYS_RUN_HASWELL
#define SIMDJSON_BUILTIN_IMPLEMENTATION haswell
#elif SIMDJSON_CAN_ALWAYS_RUN_WESTMERE
#define SIMDJSON_BUILTIN_IMPLEMENTATION westmere
#elif SIMDJSON_CAN_ALWAYS_RUN_ARM64
#define SIMDJSON_BUILTIN_IMPLEMENTATION arm64
#elif SIMDJSON_CAN_ALWAYS_RUN_PPC64
#define SIMDJSON_BUILTIN_IMPLEMENTATION ppc64
#elif SIMDJSON_CAN_ALWAYS_RUN_FALLBACK
#define SIMDJSON_BUILTIN_IMPLEMENTATION fallback
#else
#error "All possible implementations (including fallback) have been disabled! simdjson will not run."
#endif
#endif // SIMDJSON_BUILTIN_IMPLEMENTATION

// redefining SIMDJSON_IMPLEMENTATION to "SIMDJSON_BUILTIN_IMPLEMENTATION"
// #define SIMDJSON_IMPLEMENTATION SIMDJSON_BUILTIN_IMPLEMENTATION


namespace simdjson {
  /**
   * Represents the best statically linked simdjson implementation that can be used by the compiling
   * program.
   *
   * Detects what options the program is compiled against, and picks the minimum implementation that
   * will work on any computer that can run the program. For example, if you compile with g++
   * -march=westmere, it will pick the westmere implementation. The haswell implementation will
   * still be available, and can be selected at runtime, but the builtin implementation (and any
   * code that uses it) will use westmere.
   */
  namespace builtin = SIMDJSON_BUILTIN_IMPLEMENTATION;
  /**
   * Function which returns a pointer to an implementation matching the "builtin" implementation.
   * The builtin implementation is the best statically linked simdjson implementation that can be used by the compiling
   * program. If you compile with g++ -march=haswell, this will return the haswell implementation.
   * It is handy to be able to check what builtin was used: builtin_implementation()->name().
   */
  const implementation * builtin_implementation();
} // namespace simdjson

#endif // SIMDJSON_BUILTIN_H
/* end file include/simdjson/builtin.h */

#endif // SIMDJSON_H
/* end file include/simdjson.h */
