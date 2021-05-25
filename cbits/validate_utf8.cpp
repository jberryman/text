#include "simdjson.h"

extern "C"
int _hs_text_is_valid_utf8(const char* str, size_t len){
  return simdjson::validate_utf8(str, len);
}
