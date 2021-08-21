/*
 * Copyright (c) 2011 Bryan O'Sullivan <bos@serpentine.com>.
 */

#include <string.h>
#include <stdint.h>
#include <sys/types.h>
#include <stdio.h>

int _hs_text_memcmp(const void *a, size_t aoff, const void *b, size_t boff,
                   size_t n)
{
  return memcmp(a + aoff, b + boff, n);
}

ssize_t _hs_text_memchr_NL(const void *a, size_t aoff, size_t n)
{
  const void *ptr = memchr(a + aoff, 0x0A, n);
  return ptr == NULL ? -1 : ptr - (a + aoff);
}
