/*
 * Copyright (c) 2021 Andrew Lelechenko <andrew.lelechenko@gmail.com>
 */

#include <string.h>
#include <stdint.h>

void _hs_text_reverse(uint8_t *dst0, const uint8_t *src0, size_t off, size_t len)
{
  const uint8_t *src = src0 + off;
  const uint8_t *srcend = src + len;
  uint8_t *dst = dst0 + len - 1;

  while (src < srcend){
    uint8_t leadByte = *src++;
    if (leadByte >= 0xf0){
      *(dst-3) = leadByte;
      *(dst-2) = *src++;
      *(dst-1) = *src++;
      *dst     = *src++;
      dst-=4;
    } else if(leadByte >= 0xe0){
      *(dst-2) = leadByte;
      *(dst-1) = *src++;
      *dst     = *src++;
      dst-=3;
    } else if(leadByte >= 0xc0){
      *(dst-1) = leadByte;
      *dst     = *src++;
      dst-=2;
    } else {
      *dst-- = leadByte;
    }
  }
}
