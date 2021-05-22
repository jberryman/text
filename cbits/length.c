/*
 * Copyright (c) 2021 Andrew Lelechenko <andrew.lelechenko@gmail.com>
 */

#include <string.h>
#include <stdint.h>
#include <emmintrin.h>
#include <xmmintrin.h>
#include <immintrin.h>
#include <cpuid.h>
#include <stdbool.h>

#ifndef __STDC_NO_ATOMICS__
#include <stdatomic.h>
#endif

bool has_avx512_vl_bw() {
  uint32_t eax = 0, ebx = 0, ecx = 0, edx = 0;
  __get_cpuid_count(7, 0, &eax, &ebx, &ecx, &edx);
  // https://en.wikipedia.org/wiki/CPUID#EAX=7,_ECX=0:_Extended_Features
  const bool has_avx512_bw = ebx & (1 << 30);
  const bool has_avx512_vl = ebx & (1 << 31);
  // printf("cpuid=%d=cpuid\n", has_avx512_bw && has_avx512_vl);
  return has_avx512_bw && has_avx512_vl;
}

// Length

inline size_t length_naive(const uint8_t *src, const uint8_t *srcend)
{
  size_t cnt = 0;

  // Count leading bytes in 8 byte sequence
  while (src < srcend - 7){
    uint64_t w64;
    memcpy(&w64, src, sizeof(uint64_t));
    size_t leads = __builtin_popcountll(((w64 << 1) | ~w64) & 0x8080808080808080ULL);
    src+= 8;
    cnt+= leads;
  }

  // Skip until next leading byte
  while (src < srcend){
    uint8_t w8 = *src;
    if ((int8_t)w8 >= -0x40) break;
    src++;
  }

  // Finish up with tail
  while (src < srcend){
    uint8_t leadByte = *src++;
    src+= (leadByte >= 0xc0) + (leadByte >= 0xe0) + (leadByte >= 0xf0);
    cnt++;
  }

  return cnt;
}

__attribute__((target("avx512vl,avx512bw")))
size_t length_avx(const uint8_t *src, const uint8_t *srcend)
{
  size_t cnt = 0;

  while (src < srcend - 63){
    __m512i w512 = _mm512_loadu_si512((__m512i *)src);
    // Which bytes are either < 128 or >= 192?
    uint64_t mask = _mm512_cmpgt_epi8_mask(w512, _mm512_set1_epi8(0xBF));
    size_t leads = __builtin_popcountll(mask);
    cnt+= leads;
    src+= 64;
  }

  // Cannot proceed to length_sse, because of AVX-SSE transition penalties
  // https://software.intel.com/content/www/us/en/develop/articles/avoiding-avx-sse-transition-penalties.html

  if (src < srcend - 31){
    __m256i w256 = _mm256_loadu_si256((__m256i *)src);
    uint32_t mask = _mm256_cmpgt_epi8_mask(w256, _mm256_set1_epi8(0xBF));
    size_t leads = __builtin_popcountl(mask);
    cnt+= leads;
    src+= 32;
  }

  if (src < srcend - 15){
    __m128i w128 = _mm_maskz_loadu_epi16(0xFF, (__m128i *)src); // not _mm_loadu_si128; and GCC does not have _mm_loadu_epi16
    uint16_t mask = _mm_cmpgt_epi8_mask(w128, _mm_set1_epi8(0xBF)); // not _mm_movemask_epi8
    size_t leads = __builtin_popcountl(mask);
    cnt+= leads;
    src+= 16;
  }

  return cnt + length_naive(src, srcend);
}

size_t length_sse(const uint8_t *src, const uint8_t *srcend)
{
  size_t cnt = 0;

#if defined(__x86_64__)
  while (src < srcend - 15){
    __m128i w128 = _mm_loadu_si128((__m128i *)src);
    // Which bytes are either < 128 or >= 192?
    uint16_t mask = _mm_movemask_epi8(_mm_cmpgt_epi8(w128, _mm_set1_epi8(0xBF)));
    size_t leads = __builtin_popcount(mask);
    cnt+= leads;
    src+= 16;
  }
#endif

  return cnt + length_naive(src, srcend);
}

typedef size_t (*length_t) (const uint8_t*, const uint8_t*);

size_t _hs_text_length(const uint8_t *src, size_t off, size_t len) {
  static _Atomic length_t s_impl = (length_t)NULL;
  length_t impl = atomic_load_explicit(&s_impl, memory_order_relaxed);
  if (!impl) {
    impl = has_avx512_vl_bw() ? length_avx : length_sse;
    atomic_store_explicit(&s_impl, impl, memory_order_relaxed);
  }
  return (*impl)(src + off, src + off + len);
}

// Iterate

inline const uint8_t * iterate_naive(const uint8_t *src, const uint8_t *srcend, size_t cnt)
{
  // Count leading bytes in 8 byte sequence
  while (src < srcend - 7){
    uint64_t w64;
    memcpy(&w64, src, sizeof(uint64_t));
    size_t leads = __builtin_popcountll(((w64 << 1) | ~w64) & 0x8080808080808080ULL);
    if (cnt < leads) break;
    cnt-= leads;
    src+= 8;
  }

  // Skip until next leading byte
  while (src < srcend){
    uint8_t w8 = *src;
    if ((int8_t)w8 >= -0x40) break;
    src++;
  }

  // Finish up with tail
  while (src < srcend && cnt > 0){
    uint8_t leadByte = *src++;
    cnt--;
    src+= (leadByte >= 0xc0) + (leadByte >= 0xe0) + (leadByte >= 0xf0);
  }

  return src;
}

__attribute__((target("avx512vl,avx512bw")))
const uint8_t * iterate_avx(const uint8_t *src, const uint8_t *srcend, size_t cnt)
{
  while (src < srcend - 63){
    __m512i w512 = _mm512_loadu_si512((__m512i *)src);
    // Which bytes are either < 128 or >= 192?
    uint64_t mask = _mm512_cmpgt_epi8_mask(w512, _mm512_set1_epi8(0xBF));
    size_t leads = __builtin_popcountll(mask);
    if (cnt < leads) break;
    cnt-= leads;
    src+= 64;
  }

  // Cannot proceed to iterate_sse, because of AVX-SSE transition penalties
  // https://software.intel.com/content/www/us/en/develop/articles/avoiding-avx-sse-transition-penalties.html

  if (src < srcend - 31){
    __m256i w256 = _mm256_loadu_si256((__m256i *)src);
    uint32_t mask = _mm256_cmpgt_epi8_mask(w256, _mm256_set1_epi8(0xBF));
    size_t leads = __builtin_popcountl(mask);
    if (cnt >= leads){
      cnt-= leads;
      src+= 32;
    }
  }

  if (src < srcend - 15){
    __m128i w128 = _mm_maskz_loadu_epi16(0xFF, (__m128i *)src); // not _mm_loadu_si128; and GCC does not have _mm_loadu_epi16
    uint16_t mask = _mm_cmpgt_epi8_mask(w128, _mm_set1_epi8(0xBF)); // not _mm_movemask_epi8
    size_t leads = __builtin_popcountl(mask);
    if (cnt >= leads){
      cnt-= leads;
      src+= 16;
    }
  }

  return iterate_naive(src, srcend, cnt);
}

const uint8_t * iterate_sse(const uint8_t *src, const uint8_t *srcend, size_t cnt)
{
#if defined(__x86_64__)
  while (src < srcend - 15){
    __m128i w128 = _mm_loadu_si128((__m128i *)src);
    // Which bytes are either < 128 or >= 192?
    uint16_t mask = _mm_movemask_epi8(_mm_cmpgt_epi8(w128, _mm_set1_epi8(0xBF)));
    size_t leads = __builtin_popcount(mask);
    if (cnt < leads) break;
    cnt-= leads;
    src+= 16;
  }
#endif

  return iterate_naive(src, srcend, cnt);
}

typedef const uint8_t* (*iterate_t) (const uint8_t*, const uint8_t*, size_t);

size_t _hs_text_iterate(const uint8_t *src, size_t off, size_t len, size_t cnt) {
  static _Atomic iterate_t s_impl = (iterate_t)NULL;
  iterate_t impl = atomic_load_explicit(&s_impl, memory_order_relaxed);
  if (!impl) {
    impl = has_avx512_vl_bw() ? iterate_avx : iterate_sse;
    atomic_store_explicit(&s_impl, impl, memory_order_relaxed);
  }
  return (*impl)(src + off, src + off + len, cnt) - src - off;
}
