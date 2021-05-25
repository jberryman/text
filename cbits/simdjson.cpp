/* auto-generated on 2021-05-27 22:44:11 +0100. Do not edit! */
/* begin file src/simdjson.cpp */
#include "simdjson.h"

SIMDJSON_PUSH_DISABLE_WARNINGS
SIMDJSON_DISABLE_UNDESIRED_WARNINGS

/* begin file src/implementation.cpp */
#include <initializer_list>

namespace simdjson {

bool implementation::supported_by_runtime_system() const {
  uint32_t required_instruction_sets = this->required_instruction_sets();
  uint32_t supported_instruction_sets = internal::detect_supported_architectures();
  return ((supported_instruction_sets & required_instruction_sets) == required_instruction_sets);
}

namespace internal {

// Static array of known implementations. We're hoping these get baked into the executable
// without requiring a static initializer.

#if SIMDJSON_IMPLEMENTATION_HASWELL
const haswell::implementation haswell_singleton{};
#endif
#if SIMDJSON_IMPLEMENTATION_WESTMERE
const westmere::implementation westmere_singleton{};
#endif // SIMDJSON_IMPLEMENTATION_WESTMERE
#if SIMDJSON_IMPLEMENTATION_ARM64
const arm64::implementation arm64_singleton{};
#endif // SIMDJSON_IMPLEMENTATION_ARM64
#if SIMDJSON_IMPLEMENTATION_PPC64
const ppc64::implementation ppc64_singleton{};
#endif // SIMDJSON_IMPLEMENTATION_PPC64
#if SIMDJSON_IMPLEMENTATION_FALLBACK
const fallback::implementation fallback_singleton{};
#endif // SIMDJSON_IMPLEMENTATION_FALLBACK

/**
 * @private Detects best supported implementation on first use, and sets it
 */
class detect_best_supported_implementation_on_first_use final : public implementation {
public:
  const std::string &name() const noexcept final { return set_best()->name(); }
  const std::string &description() const noexcept final { return set_best()->description(); }
  uint32_t required_instruction_sets() const noexcept final { return set_best()->required_instruction_sets(); }
  simdjson_warn_unused bool validate_utf8(const char * buf, size_t len) const noexcept final override {
    return set_best()->validate_utf8(buf, len);
  }
  simdjson_really_inline detect_best_supported_implementation_on_first_use() noexcept : implementation("best_supported_detector", "Detects the best supported implementation and sets it", 0) {}
private:
  const implementation *set_best() const noexcept;
};

const detect_best_supported_implementation_on_first_use detect_best_supported_implementation_on_first_use_singleton;

const std::initializer_list<const implementation *> available_implementation_pointers {
#if SIMDJSON_IMPLEMENTATION_HASWELL
  &haswell_singleton,
#endif
#if SIMDJSON_IMPLEMENTATION_WESTMERE
  &westmere_singleton,
#endif
#if SIMDJSON_IMPLEMENTATION_ARM64
  &arm64_singleton,
#endif
#if SIMDJSON_IMPLEMENTATION_PPC64
  &ppc64_singleton,
#endif
#if SIMDJSON_IMPLEMENTATION_FALLBACK
  &fallback_singleton,
#endif
}; // available_implementation_pointers

// So we can return UNSUPPORTED_ARCHITECTURE from the parser when there is no support
class unsupported_implementation final : public implementation {
public:
  simdjson_warn_unused bool validate_utf8(const char *, size_t) const noexcept final override {
    return false; // Just refuse to validate. Given that we have a fallback implementation
    // it seems unlikely that unsupported_implementation will ever be used. If it is used,
    // then it will flag all strings as invalid. The alternative is to return an error_code
    // from which the user has to figure out whether the string is valid UTF-8... which seems
    // like a lot of work just to handle the very unlikely case that we have an unsupported
    // implementation. And, when it does happen (that we have an unsupported implementation),
    // what are the chances that the programmer has a fallback? Given that *we* provide the
    // fallback, it implies that the programmer would need a fallback for our fallback.
  }
  unsupported_implementation() : implementation("unsupported", "Unsupported CPU (no detected SIMD instructions)", 0) {}
};

const unsupported_implementation unsupported_singleton{};

size_t available_implementation_list::size() const noexcept {
  return internal::available_implementation_pointers.size();
}
const implementation * const *available_implementation_list::begin() const noexcept {
  return internal::available_implementation_pointers.begin();
}
const implementation * const *available_implementation_list::end() const noexcept {
  return internal::available_implementation_pointers.end();
}
const implementation *available_implementation_list::detect_best_supported() const noexcept {
  // They are prelisted in priority order, so we just go down the list
  uint32_t supported_instruction_sets = internal::detect_supported_architectures();
  for (const implementation *impl : internal::available_implementation_pointers) {
    uint32_t required_instruction_sets = impl->required_instruction_sets();
    if ((supported_instruction_sets & required_instruction_sets) == required_instruction_sets) { return impl; }
  }
  return &unsupported_singleton; // this should never happen?
}

const implementation *detect_best_supported_implementation_on_first_use::set_best() const noexcept {
  SIMDJSON_PUSH_DISABLE_WARNINGS
  SIMDJSON_DISABLE_DEPRECATED_WARNING // Disable CRT_SECURE warning on MSVC: manually verified this is safe
  char *force_implementation_name = getenv("SIMDJSON_FORCE_IMPLEMENTATION");
  SIMDJSON_POP_DISABLE_WARNINGS

  if (force_implementation_name) {
    auto force_implementation = available_implementations[force_implementation_name];
    if (force_implementation) {
      return active_implementation = force_implementation;
    } else {
      // Note: abort() and stderr usage within the library is forbidden.
      return active_implementation = &unsupported_singleton;
    }
  }
  return active_implementation = available_implementations.detect_best_supported();
}

} // namespace internal

SIMDJSON_DLLIMPORTEXPORT const internal::available_implementation_list available_implementations{};
SIMDJSON_DLLIMPORTEXPORT internal::atomic_ptr<const implementation> active_implementation{&internal::detect_best_supported_implementation_on_first_use_singleton};

simdjson_warn_unused bool validate_utf8(const char *buf, size_t len) noexcept {
  return active_implementation->validate_utf8(buf, len);
}

const implementation * builtin_implementation() {
  static const implementation * builtin_impl = available_implementations[STRINGIFY(SIMDJSON_BUILTIN_IMPLEMENTATION)];
  assert(builtin_impl);
  return builtin_impl;
}


} // namespace simdjson
/* end file src/implementation.cpp */

#if SIMDJSON_IMPLEMENTATION_ARM64
/* begin file src/arm64/dom_parser_implementation.cpp */
/* begin file include/simdjson/arm64/begin.h */
// redefining SIMDJSON_IMPLEMENTATION to "arm64"
// #define SIMDJSON_IMPLEMENTATION arm64
/* end file include/simdjson/arm64/begin.h */

//
// Stage 1
//
namespace simdjson {
namespace arm64 {
namespace {

using namespace simd;

simdjson_really_inline bool is_ascii(const simd8x64<uint8_t>& input) {
    simd8<uint8_t> bits = input.reduce_or();
    return bits.max_val() < 0b10000000u;
}

simdjson_unused simdjson_really_inline simd8<bool> must_be_continuation(const simd8<uint8_t> prev1, const simd8<uint8_t> prev2, const simd8<uint8_t> prev3) {
    simd8<bool> is_second_byte = prev1 >= uint8_t(0b11000000u);
    simd8<bool> is_third_byte  = prev2 >= uint8_t(0b11100000u);
    simd8<bool> is_fourth_byte = prev3 >= uint8_t(0b11110000u);
    // Use ^ instead of | for is_*_byte, because ^ is commutative, and the caller is using ^ as well.
    // This will work fine because we only have to report errors for cases with 0-1 lead bytes.
    // Multiple lead bytes implies 2 overlapping multibyte characters, and if that happens, there is
    // guaranteed to be at least *one* lead byte that is part of only 1 other multibyte character.
    // The error will be detected there.
    return is_second_byte ^ is_third_byte ^ is_fourth_byte;
}

simdjson_really_inline simd8<bool> must_be_2_3_continuation(const simd8<uint8_t> prev2, const simd8<uint8_t> prev3) {
    simd8<bool> is_third_byte  = prev2 >= uint8_t(0b11100000u);
    simd8<bool> is_fourth_byte = prev3 >= uint8_t(0b11110000u);
    return is_third_byte ^ is_fourth_byte;
}

} // unnamed namespace
} // namespace arm64
} // namespace simdjson

/* begin file src/generic/stage1/utf8_lookup4_algorithm.h */
namespace simdjson {
namespace arm64 {
namespace {
namespace utf8_validation {

using namespace simd;

  simdjson_really_inline simd8<uint8_t> check_special_cases(const simd8<uint8_t> input, const simd8<uint8_t> prev1) {
// Bit 0 = Too Short (lead byte/ASCII followed by lead byte/ASCII)
// Bit 1 = Too Long (ASCII followed by continuation)
// Bit 2 = Overlong 3-byte
// Bit 4 = Surrogate
// Bit 5 = Overlong 2-byte
// Bit 7 = Two Continuations
    constexpr const uint8_t TOO_SHORT   = 1<<0; // 11______ 0_______
                                                // 11______ 11______
    constexpr const uint8_t TOO_LONG    = 1<<1; // 0_______ 10______
    constexpr const uint8_t OVERLONG_3  = 1<<2; // 11100000 100_____
    constexpr const uint8_t SURROGATE   = 1<<4; // 11101101 101_____
    constexpr const uint8_t OVERLONG_2  = 1<<5; // 1100000_ 10______
    constexpr const uint8_t TWO_CONTS   = 1<<7; // 10______ 10______
    constexpr const uint8_t TOO_LARGE   = 1<<3; // 11110100 1001____
                                                // 11110100 101_____
                                                // 11110101 1001____
                                                // 11110101 101_____
                                                // 1111011_ 1001____
                                                // 1111011_ 101_____
                                                // 11111___ 1001____
                                                // 11111___ 101_____
    constexpr const uint8_t TOO_LARGE_1000 = 1<<6;
                                                // 11110101 1000____
                                                // 1111011_ 1000____
                                                // 11111___ 1000____
    constexpr const uint8_t OVERLONG_4  = 1<<6; // 11110000 1000____

    const simd8<uint8_t> byte_1_high = prev1.shr<4>().lookup_16<uint8_t>(
      // 0_______ ________ <ASCII in byte 1>
      TOO_LONG, TOO_LONG, TOO_LONG, TOO_LONG,
      TOO_LONG, TOO_LONG, TOO_LONG, TOO_LONG,
      // 10______ ________ <continuation in byte 1>
      TWO_CONTS, TWO_CONTS, TWO_CONTS, TWO_CONTS,
      // 1100____ ________ <two byte lead in byte 1>
      TOO_SHORT | OVERLONG_2,
      // 1101____ ________ <two byte lead in byte 1>
      TOO_SHORT,
      // 1110____ ________ <three byte lead in byte 1>
      TOO_SHORT | OVERLONG_3 | SURROGATE,
      // 1111____ ________ <four+ byte lead in byte 1>
      TOO_SHORT | TOO_LARGE | TOO_LARGE_1000 | OVERLONG_4
    );
    constexpr const uint8_t CARRY = TOO_SHORT | TOO_LONG | TWO_CONTS; // These all have ____ in byte 1 .
    const simd8<uint8_t> byte_1_low = (prev1 & 0x0F).lookup_16<uint8_t>(
      // ____0000 ________
      CARRY | OVERLONG_3 | OVERLONG_2 | OVERLONG_4,
      // ____0001 ________
      CARRY | OVERLONG_2,
      // ____001_ ________
      CARRY,
      CARRY,

      // ____0100 ________
      CARRY | TOO_LARGE,
      // ____0101 ________
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      // ____011_ ________
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,

      // ____1___ ________
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      // ____1101 ________
      CARRY | TOO_LARGE | TOO_LARGE_1000 | SURROGATE,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000
    );
    const simd8<uint8_t> byte_2_high = input.shr<4>().lookup_16<uint8_t>(
      // ________ 0_______ <ASCII in byte 2>
      TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,
      TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,

      // ________ 1000____
      TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE_1000 | OVERLONG_4,
      // ________ 1001____
      TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE,
      // ________ 101_____
      TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE  | TOO_LARGE,
      TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE  | TOO_LARGE,

      // ________ 11______
      TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT
    );
    return (byte_1_high & byte_1_low & byte_2_high);
  }
  simdjson_really_inline simd8<uint8_t> check_multibyte_lengths(const simd8<uint8_t> input,
      const simd8<uint8_t> prev_input, const simd8<uint8_t> sc) {
    simd8<uint8_t> prev2 = input.prev<2>(prev_input);
    simd8<uint8_t> prev3 = input.prev<3>(prev_input);
    simd8<uint8_t> must23 = simd8<uint8_t>(must_be_2_3_continuation(prev2, prev3));
    simd8<uint8_t> must23_80 = must23 & uint8_t(0x80);
    return must23_80 ^ sc;
  }

  //
  // Return nonzero if there are incomplete multibyte characters at the end of the block:
  // e.g. if there is a 4-byte character, but it's 3 bytes from the end.
  //
  simdjson_really_inline simd8<uint8_t> is_incomplete(const simd8<uint8_t> input) {
    // If the previous input's last 3 bytes match this, they're too short (they ended at EOF):
    // ... 1111____ 111_____ 11______
    static const uint8_t max_array[32] = {
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 0b11110000u-1, 0b11100000u-1, 0b11000000u-1
    };
    const simd8<uint8_t> max_value(&max_array[sizeof(max_array)-sizeof(simd8<uint8_t>)]);
    return input.gt_bits(max_value);
  }

  struct utf8_checker {
    // If this is nonzero, there has been a UTF-8 error.
    simd8<uint8_t> error;
    // The last input we received
    simd8<uint8_t> prev_input_block;
    // Whether the last input we received was incomplete (used for ASCII fast path)
    simd8<uint8_t> prev_incomplete;

    //
    // Check whether the current bytes are valid UTF-8.
    //
    simdjson_really_inline void check_utf8_bytes(const simd8<uint8_t> input, const simd8<uint8_t> prev_input) {
      // Flip prev1...prev3 so we can easily determine if they are 2+, 3+ or 4+ lead bytes
      // (2, 3, 4-byte leads become large positive numbers instead of small negative numbers)
      simd8<uint8_t> prev1 = input.prev<1>(prev_input);
      simd8<uint8_t> sc = check_special_cases(input, prev1);
      this->error |= check_multibyte_lengths(input, prev_input, sc);
    }

    // The only problem that can happen at EOF is that a multibyte character is too short
    // or a byte value too large in the last bytes: check_special_cases only checks for bytes
    // too large in the first of two bytes.
    simdjson_really_inline void check_eof() {
      // If the previous block had incomplete UTF-8 characters at the end, an ASCII block can't
      // possibly finish them.
      this->error |= this->prev_incomplete;
    }

    simdjson_really_inline void check_next_input(const simd8x64<uint8_t>& input) {
      if(simdjson_likely(is_ascii(input))) {
        this->error |= this->prev_incomplete;
      } else {
        // you might think that a for-loop would work, but under Visual Studio, it is not good enough.
        static_assert((simd8x64<uint8_t>::NUM_CHUNKS == 2) || (simd8x64<uint8_t>::NUM_CHUNKS == 4),
            "We support either two or four chunks per 64-byte block.");
        if(simd8x64<uint8_t>::NUM_CHUNKS == 2) {
          this->check_utf8_bytes(input.chunks[0], this->prev_input_block);
          this->check_utf8_bytes(input.chunks[1], input.chunks[0]);
        } else if(simd8x64<uint8_t>::NUM_CHUNKS == 4) {
          this->check_utf8_bytes(input.chunks[0], this->prev_input_block);
          this->check_utf8_bytes(input.chunks[1], input.chunks[0]);
          this->check_utf8_bytes(input.chunks[2], input.chunks[1]);
          this->check_utf8_bytes(input.chunks[3], input.chunks[2]);
        }
        this->prev_incomplete = is_incomplete(input.chunks[simd8x64<uint8_t>::NUM_CHUNKS-1]);
        this->prev_input_block = input.chunks[simd8x64<uint8_t>::NUM_CHUNKS-1];

      }
    }
    // do not forget to call check_eof!
    simdjson_really_inline error_code errors() {
      return this->error.any_bits_set_anywhere() ? error_code::UTF8_ERROR : error_code::SUCCESS;
    }

  }; // struct utf8_checker
} // namespace utf8_validation

using utf8_validation::utf8_checker;

} // unnamed namespace
} // namespace arm64
} // namespace simdjson
/* end file src/generic/stage1/utf8_lookup4_algorithm.h */
/* begin file src/generic/stage1/utf8_validator.h */
namespace simdjson {
namespace arm64 {
namespace {
namespace stage1 {

/**
 * Validates that the string is actual UTF-8.
 */
template<class checker>
bool generic_validate_utf8(const uint8_t * input, size_t length) {
    checker c{};
    buf_block_reader<64> reader(input, length);
    while (reader.has_full_block()) {
      simd::simd8x64<uint8_t> in(reader.full_block());
      c.check_next_input(in);
      reader.advance();
    }
    uint8_t block[64]{};
    reader.get_remainder(block);
    simd::simd8x64<uint8_t> in(block);
    c.check_next_input(in);
    reader.advance();
    c.check_eof();
    return c.errors() == error_code::SUCCESS;
}

bool generic_validate_utf8(const char * input, size_t length) {
    return generic_validate_utf8<utf8_checker>(reinterpret_cast<const uint8_t *>(input),length);
}

} // namespace stage1
} // unnamed namespace
} // namespace arm64
} // namespace simdjson
/* end file src/generic/stage1/utf8_validator.h */

//
// Implementation-specific overrides
//
namespace simdjson {
namespace arm64 {

simdjson_warn_unused bool implementation::validate_utf8(const char *buf, size_t len) const noexcept {
  return arm64::stage1::generic_validate_utf8(buf,len);
}

} // namespace arm64
} // namespace simdjson

/* begin file include/simdjson/arm64/end.h */
/* end file include/simdjson/arm64/end.h */
/* end file src/arm64/dom_parser_implementation.cpp */
#endif
#if SIMDJSON_IMPLEMENTATION_FALLBACK
/* begin file src/fallback/dom_parser_implementation.cpp */
#include <cstring>
/* begin file include/simdjson/fallback/begin.h */
// redefining SIMDJSON_IMPLEMENTATION to "fallback"
// #define SIMDJSON_IMPLEMENTATION fallback
/* end file include/simdjson/fallback/begin.h */

//
// Stage 1
//

namespace simdjson {
namespace fallback {

// credit: based on code from Google Fuchsia (Apache Licensed)
simdjson_warn_unused bool implementation::validate_utf8(const char *buf, size_t len) const noexcept {
  const uint8_t *data = reinterpret_cast<const uint8_t *>(buf);
  uint64_t pos = 0;
  uint32_t code_point = 0;
  while (pos < len) {
    // check of the next 8 bytes are ascii.
    uint64_t next_pos = pos + 16;
    if (next_pos <= len) { // if it is safe to read 8 more bytes, check that they are ascii
      uint64_t v1;
      memcpy(&v1, data + pos, sizeof(uint64_t));
      uint64_t v2;
      memcpy(&v2, data + pos + sizeof(uint64_t), sizeof(uint64_t));
      uint64_t v{v1 | v2};
      if ((v & 0x8080808080808080) == 0) {
        pos = next_pos;
        continue;
      }
    }
    unsigned char byte = data[pos];
    if (byte < 0b10000000) {
      pos++;
      continue;
    } else if ((byte & 0b11100000) == 0b11000000) {
      next_pos = pos + 2;
      if (next_pos > len) { return false; }
      if ((data[pos + 1] & 0b11000000) != 0b10000000) { return false; }
      // range check
      code_point = (byte & 0b00011111) << 6 | (data[pos + 1] & 0b00111111);
      if (code_point < 0x80 || 0x7ff < code_point) { return false; }
    } else if ((byte & 0b11110000) == 0b11100000) {
      next_pos = pos + 3;
      if (next_pos > len) { return false; }
      if ((data[pos + 1] & 0b11000000) != 0b10000000) { return false; }
      if ((data[pos + 2] & 0b11000000) != 0b10000000) { return false; }
      // range check
      code_point = (byte & 0b00001111) << 12 |
                   (data[pos + 1] & 0b00111111) << 6 |
                   (data[pos + 2] & 0b00111111);
      if (code_point < 0x800 || 0xffff < code_point ||
          (0xd7ff < code_point && code_point < 0xe000)) {
        return false;
      }
    } else if ((byte & 0b11111000) == 0b11110000) { // 0b11110000
      next_pos = pos + 4;
      if (next_pos > len) { return false; }
      if ((data[pos + 1] & 0b11000000) != 0b10000000) { return false; }
      if ((data[pos + 2] & 0b11000000) != 0b10000000) { return false; }
      if ((data[pos + 3] & 0b11000000) != 0b10000000) { return false; }
      // range check
      code_point =
          (byte & 0b00000111) << 18 | (data[pos + 1] & 0b00111111) << 12 |
          (data[pos + 2] & 0b00111111) << 6 | (data[pos + 3] & 0b00111111);
      if (code_point <= 0xffff || 0x10ffff < code_point) { return false; }
    } else {
      // we may have a continuation
      return false;
    }
    pos = next_pos;
  }
  return true;
}

} // namespace fallback
} // namespace simdjson

/* begin file include/simdjson/fallback/end.h */
/* end file include/simdjson/fallback/end.h */
/* end file src/fallback/dom_parser_implementation.cpp */
#endif
#if SIMDJSON_IMPLEMENTATION_HASWELL
/* begin file src/haswell/dom_parser_implementation.cpp */
/* begin file include/simdjson/haswell/begin.h */
// redefining SIMDJSON_IMPLEMENTATION to "haswell"
// #define SIMDJSON_IMPLEMENTATION haswell
SIMDJSON_TARGET_HASWELL
/* end file include/simdjson/haswell/begin.h */

//
// Stage 1
//

namespace simdjson {
namespace haswell {
namespace {

using namespace simd;

simdjson_really_inline bool is_ascii(const simd8x64<uint8_t>& input) {
  return input.reduce_or().is_ascii();
}

simdjson_really_inline simd8<bool> must_be_2_3_continuation(const simd8<uint8_t> prev2, const simd8<uint8_t> prev3) {
  simd8<uint8_t> is_third_byte  = prev2.saturating_sub(0b11100000u-1); // Only 111_____ will be > 0
  simd8<uint8_t> is_fourth_byte = prev3.saturating_sub(0b11110000u-1); // Only 1111____ will be > 0
  // Caller requires a bool (all 1's). All values resulting from the subtraction will be <= 64, so signed comparison is fine.
  return simd8<int8_t>(is_third_byte | is_fourth_byte) > int8_t(0);
}

} // unnamed namespace
} // namespace haswell
} // namespace simdjson

/* begin file src/generic/stage1/utf8_lookup4_algorithm.h */
namespace simdjson {
namespace haswell {
namespace {
namespace utf8_validation {

using namespace simd;

  simdjson_really_inline simd8<uint8_t> check_special_cases(const simd8<uint8_t> input, const simd8<uint8_t> prev1) {
// Bit 0 = Too Short (lead byte/ASCII followed by lead byte/ASCII)
// Bit 1 = Too Long (ASCII followed by continuation)
// Bit 2 = Overlong 3-byte
// Bit 4 = Surrogate
// Bit 5 = Overlong 2-byte
// Bit 7 = Two Continuations
    constexpr const uint8_t TOO_SHORT   = 1<<0; // 11______ 0_______
                                                // 11______ 11______
    constexpr const uint8_t TOO_LONG    = 1<<1; // 0_______ 10______
    constexpr const uint8_t OVERLONG_3  = 1<<2; // 11100000 100_____
    constexpr const uint8_t SURROGATE   = 1<<4; // 11101101 101_____
    constexpr const uint8_t OVERLONG_2  = 1<<5; // 1100000_ 10______
    constexpr const uint8_t TWO_CONTS   = 1<<7; // 10______ 10______
    constexpr const uint8_t TOO_LARGE   = 1<<3; // 11110100 1001____
                                                // 11110100 101_____
                                                // 11110101 1001____
                                                // 11110101 101_____
                                                // 1111011_ 1001____
                                                // 1111011_ 101_____
                                                // 11111___ 1001____
                                                // 11111___ 101_____
    constexpr const uint8_t TOO_LARGE_1000 = 1<<6;
                                                // 11110101 1000____
                                                // 1111011_ 1000____
                                                // 11111___ 1000____
    constexpr const uint8_t OVERLONG_4  = 1<<6; // 11110000 1000____

    const simd8<uint8_t> byte_1_high = prev1.shr<4>().lookup_16<uint8_t>(
      // 0_______ ________ <ASCII in byte 1>
      TOO_LONG, TOO_LONG, TOO_LONG, TOO_LONG,
      TOO_LONG, TOO_LONG, TOO_LONG, TOO_LONG,
      // 10______ ________ <continuation in byte 1>
      TWO_CONTS, TWO_CONTS, TWO_CONTS, TWO_CONTS,
      // 1100____ ________ <two byte lead in byte 1>
      TOO_SHORT | OVERLONG_2,
      // 1101____ ________ <two byte lead in byte 1>
      TOO_SHORT,
      // 1110____ ________ <three byte lead in byte 1>
      TOO_SHORT | OVERLONG_3 | SURROGATE,
      // 1111____ ________ <four+ byte lead in byte 1>
      TOO_SHORT | TOO_LARGE | TOO_LARGE_1000 | OVERLONG_4
    );
    constexpr const uint8_t CARRY = TOO_SHORT | TOO_LONG | TWO_CONTS; // These all have ____ in byte 1 .
    const simd8<uint8_t> byte_1_low = (prev1 & 0x0F).lookup_16<uint8_t>(
      // ____0000 ________
      CARRY | OVERLONG_3 | OVERLONG_2 | OVERLONG_4,
      // ____0001 ________
      CARRY | OVERLONG_2,
      // ____001_ ________
      CARRY,
      CARRY,

      // ____0100 ________
      CARRY | TOO_LARGE,
      // ____0101 ________
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      // ____011_ ________
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,

      // ____1___ ________
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      // ____1101 ________
      CARRY | TOO_LARGE | TOO_LARGE_1000 | SURROGATE,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000
    );
    const simd8<uint8_t> byte_2_high = input.shr<4>().lookup_16<uint8_t>(
      // ________ 0_______ <ASCII in byte 2>
      TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,
      TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,

      // ________ 1000____
      TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE_1000 | OVERLONG_4,
      // ________ 1001____
      TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE,
      // ________ 101_____
      TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE  | TOO_LARGE,
      TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE  | TOO_LARGE,

      // ________ 11______
      TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT
    );
    return (byte_1_high & byte_1_low & byte_2_high);
  }
  simdjson_really_inline simd8<uint8_t> check_multibyte_lengths(const simd8<uint8_t> input,
      const simd8<uint8_t> prev_input, const simd8<uint8_t> sc) {
    simd8<uint8_t> prev2 = input.prev<2>(prev_input);
    simd8<uint8_t> prev3 = input.prev<3>(prev_input);
    simd8<uint8_t> must23 = simd8<uint8_t>(must_be_2_3_continuation(prev2, prev3));
    simd8<uint8_t> must23_80 = must23 & uint8_t(0x80);
    return must23_80 ^ sc;
  }

  //
  // Return nonzero if there are incomplete multibyte characters at the end of the block:
  // e.g. if there is a 4-byte character, but it's 3 bytes from the end.
  //
  simdjson_really_inline simd8<uint8_t> is_incomplete(const simd8<uint8_t> input) {
    // If the previous input's last 3 bytes match this, they're too short (they ended at EOF):
    // ... 1111____ 111_____ 11______
    static const uint8_t max_array[32] = {
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 0b11110000u-1, 0b11100000u-1, 0b11000000u-1
    };
    const simd8<uint8_t> max_value(&max_array[sizeof(max_array)-sizeof(simd8<uint8_t>)]);
    return input.gt_bits(max_value);
  }

  struct utf8_checker {
    // If this is nonzero, there has been a UTF-8 error.
    simd8<uint8_t> error;
    // The last input we received
    simd8<uint8_t> prev_input_block;
    // Whether the last input we received was incomplete (used for ASCII fast path)
    simd8<uint8_t> prev_incomplete;

    //
    // Check whether the current bytes are valid UTF-8.
    //
    simdjson_really_inline void check_utf8_bytes(const simd8<uint8_t> input, const simd8<uint8_t> prev_input) {
      // Flip prev1...prev3 so we can easily determine if they are 2+, 3+ or 4+ lead bytes
      // (2, 3, 4-byte leads become large positive numbers instead of small negative numbers)
      simd8<uint8_t> prev1 = input.prev<1>(prev_input);
      simd8<uint8_t> sc = check_special_cases(input, prev1);
      this->error |= check_multibyte_lengths(input, prev_input, sc);
    }

    // The only problem that can happen at EOF is that a multibyte character is too short
    // or a byte value too large in the last bytes: check_special_cases only checks for bytes
    // too large in the first of two bytes.
    simdjson_really_inline void check_eof() {
      // If the previous block had incomplete UTF-8 characters at the end, an ASCII block can't
      // possibly finish them.
      this->error |= this->prev_incomplete;
    }

    simdjson_really_inline void check_next_input(const simd8x64<uint8_t>& input) {
      if(simdjson_likely(is_ascii(input))) {
        this->error |= this->prev_incomplete;
      } else {
        // you might think that a for-loop would work, but under Visual Studio, it is not good enough.
        static_assert((simd8x64<uint8_t>::NUM_CHUNKS == 2) || (simd8x64<uint8_t>::NUM_CHUNKS == 4),
            "We support either two or four chunks per 64-byte block.");
        if(simd8x64<uint8_t>::NUM_CHUNKS == 2) {
          this->check_utf8_bytes(input.chunks[0], this->prev_input_block);
          this->check_utf8_bytes(input.chunks[1], input.chunks[0]);
        } else if(simd8x64<uint8_t>::NUM_CHUNKS == 4) {
          this->check_utf8_bytes(input.chunks[0], this->prev_input_block);
          this->check_utf8_bytes(input.chunks[1], input.chunks[0]);
          this->check_utf8_bytes(input.chunks[2], input.chunks[1]);
          this->check_utf8_bytes(input.chunks[3], input.chunks[2]);
        }
        this->prev_incomplete = is_incomplete(input.chunks[simd8x64<uint8_t>::NUM_CHUNKS-1]);
        this->prev_input_block = input.chunks[simd8x64<uint8_t>::NUM_CHUNKS-1];

      }
    }
    // do not forget to call check_eof!
    simdjson_really_inline error_code errors() {
      return this->error.any_bits_set_anywhere() ? error_code::UTF8_ERROR : error_code::SUCCESS;
    }

  }; // struct utf8_checker
} // namespace utf8_validation

using utf8_validation::utf8_checker;

} // unnamed namespace
} // namespace haswell
} // namespace simdjson
/* end file src/generic/stage1/utf8_lookup4_algorithm.h */
/* begin file src/generic/stage1/buf_block_reader.h */
#include <cstring>

namespace simdjson {
namespace haswell {
namespace {

// Walks through a buffer in block-sized increments, loading the last part with spaces
template<size_t STEP_SIZE>
struct buf_block_reader {
public:
  simdjson_really_inline buf_block_reader(const uint8_t *_buf, size_t _len);
  simdjson_really_inline size_t block_index();
  simdjson_really_inline bool has_full_block() const;
  simdjson_really_inline const uint8_t *full_block() const;
  /**
   * Get the last block, padded with spaces.
   *
   * There will always be a last block, with at least 1 byte, unless len == 0 (in which case this
   * function fills the buffer with spaces and returns 0. In particular, if len == STEP_SIZE there
   * will be 0 full_blocks and 1 remainder block with STEP_SIZE bytes and no spaces for padding.
   *
   * @return the number of effective characters in the last block.
   */
  simdjson_really_inline size_t get_remainder(uint8_t *dst) const;
  simdjson_really_inline void advance();
private:
  const uint8_t *buf;
  const size_t len;
  const size_t lenminusstep;
  size_t idx;
};

// Routines to print masks and text for debugging bitmask operations
simdjson_unused static char * format_input_text_64(const uint8_t *text) {
  static char buf[sizeof(simd8x64<uint8_t>) + 1];
  for (size_t i=0; i<sizeof(simd8x64<uint8_t>); i++) {
    buf[i] = int8_t(text[i]) < ' ' ? '_' : int8_t(text[i]);
  }
  buf[sizeof(simd8x64<uint8_t>)] = '\0';
  return buf;
}

// Routines to print masks and text for debugging bitmask operations
simdjson_unused static char * format_input_text(const simd8x64<uint8_t>& in) {
  static char buf[sizeof(simd8x64<uint8_t>) + 1];
  in.store(reinterpret_cast<uint8_t*>(buf));
  for (size_t i=0; i<sizeof(simd8x64<uint8_t>); i++) {
    if (buf[i] < ' ') { buf[i] = '_'; }
  }
  buf[sizeof(simd8x64<uint8_t>)] = '\0';
  return buf;
}

simdjson_unused static char * format_mask(uint64_t mask) {
  static char buf[sizeof(simd8x64<uint8_t>) + 1];
  for (size_t i=0; i<64; i++) {
    buf[i] = (mask & (size_t(1) << i)) ? 'X' : ' ';
  }
  buf[64] = '\0';
  return buf;
}

template<size_t STEP_SIZE>
simdjson_really_inline buf_block_reader<STEP_SIZE>::buf_block_reader(const uint8_t *_buf, size_t _len) : buf{_buf}, len{_len}, lenminusstep{len < STEP_SIZE ? 0 : len - STEP_SIZE}, idx{0} {}

template<size_t STEP_SIZE>
simdjson_really_inline size_t buf_block_reader<STEP_SIZE>::block_index() { return idx; }

template<size_t STEP_SIZE>
simdjson_really_inline bool buf_block_reader<STEP_SIZE>::has_full_block() const {
  return idx < lenminusstep;
}

template<size_t STEP_SIZE>
simdjson_really_inline const uint8_t *buf_block_reader<STEP_SIZE>::full_block() const {
  return &buf[idx];
}

template<size_t STEP_SIZE>
simdjson_really_inline size_t buf_block_reader<STEP_SIZE>::get_remainder(uint8_t *dst) const {
  if(len == idx) { return 0; } // memcpy(dst, null, 0) will trigger an error with some sanitizers
  std::memset(dst, 0x20, STEP_SIZE); // std::memset STEP_SIZE because it's more efficient to write out 8 or 16 bytes at once.
  std::memcpy(dst, buf + idx, len - idx);
  return len - idx;
}

template<size_t STEP_SIZE>
simdjson_really_inline void buf_block_reader<STEP_SIZE>::advance() {
  idx += STEP_SIZE;
}

} // unnamed namespace
} // namespace haswell
} // namespace simdjson
/* end file src/generic/stage1/buf_block_reader.h */
/* begin file src/generic/stage1/utf8_validator.h */
namespace simdjson {
namespace haswell {
namespace {
namespace stage1 {

/**
 * Validates that the string is actual UTF-8.
 */
template<class checker>
bool generic_validate_utf8(const uint8_t * input, size_t length) {
    checker c{};
    buf_block_reader<64> reader(input, length);
    while (reader.has_full_block()) {
      simd::simd8x64<uint8_t> in(reader.full_block());
      c.check_next_input(in);
      reader.advance();
    }
    uint8_t block[64]{};
    reader.get_remainder(block);
    simd::simd8x64<uint8_t> in(block);
    c.check_next_input(in);
    reader.advance();
    c.check_eof();
    return c.errors() == error_code::SUCCESS;
}

bool generic_validate_utf8(const char * input, size_t length) {
    return generic_validate_utf8<utf8_checker>(reinterpret_cast<const uint8_t *>(input),length);
}

} // namespace stage1
} // unnamed namespace
} // namespace haswell
} // namespace simdjson
/* end file src/generic/stage1/utf8_validator.h */

//
// Implementation-specific overrides
//
namespace simdjson {
namespace haswell {

simdjson_warn_unused bool implementation::validate_utf8(const char *buf, size_t len) const noexcept
{
  return haswell::stage1::generic_validate_utf8(buf,len);
}

} // namespace haswell
} // namespace simdjson

/* begin file include/simdjson/haswell/end.h */
SIMDJSON_UNTARGET_HASWELL
/* end file include/simdjson/haswell/end.h */
/* end file src/haswell/dom_parser_implementation.cpp */
#endif
#if SIMDJSON_IMPLEMENTATION_PPC64
/* begin file src/ppc64/dom_parser_implementation.cpp */
/* begin file include/simdjson/ppc64/begin.h */
// redefining SIMDJSON_IMPLEMENTATION to "ppc64"
// #define SIMDJSON_IMPLEMENTATION ppc64
/* end file include/simdjson/ppc64/begin.h */

//
// Stage 1
//
namespace simdjson {
namespace ppc64 {
namespace {

using namespace simd;

simdjson_really_inline bool is_ascii(const simd8x64<uint8_t>& input) {
  // careful: 0x80 is not ascii.
  return input.reduce_or().saturating_sub(0b01111111u).bits_not_set_anywhere();
}

simdjson_really_inline simd8<bool> must_be_2_3_continuation(const simd8<uint8_t> prev2, const simd8<uint8_t> prev3) {
  simd8<uint8_t> is_third_byte  = prev2.saturating_sub(0b11100000u-1); // Only 111_____ will be > 0
  simd8<uint8_t> is_fourth_byte = prev3.saturating_sub(0b11110000u-1); // Only 1111____ will be > 0
  // Caller requires a bool (all 1's). All values resulting from the subtraction will be <= 64, so signed comparison is fine.
  return simd8<int8_t>(is_third_byte | is_fourth_byte) > int8_t(0);
}

} // unnamed namespace
} // namespace ppc64
} // namespace simdjson

/* begin file src/generic/stage1/utf8_lookup4_algorithm.h */
namespace simdjson {
namespace ppc64 {
namespace {
namespace utf8_validation {

using namespace simd;

  simdjson_really_inline simd8<uint8_t> check_special_cases(const simd8<uint8_t> input, const simd8<uint8_t> prev1) {
// Bit 0 = Too Short (lead byte/ASCII followed by lead byte/ASCII)
// Bit 1 = Too Long (ASCII followed by continuation)
// Bit 2 = Overlong 3-byte
// Bit 4 = Surrogate
// Bit 5 = Overlong 2-byte
// Bit 7 = Two Continuations
    constexpr const uint8_t TOO_SHORT   = 1<<0; // 11______ 0_______
                                                // 11______ 11______
    constexpr const uint8_t TOO_LONG    = 1<<1; // 0_______ 10______
    constexpr const uint8_t OVERLONG_3  = 1<<2; // 11100000 100_____
    constexpr const uint8_t SURROGATE   = 1<<4; // 11101101 101_____
    constexpr const uint8_t OVERLONG_2  = 1<<5; // 1100000_ 10______
    constexpr const uint8_t TWO_CONTS   = 1<<7; // 10______ 10______
    constexpr const uint8_t TOO_LARGE   = 1<<3; // 11110100 1001____
                                                // 11110100 101_____
                                                // 11110101 1001____
                                                // 11110101 101_____
                                                // 1111011_ 1001____
                                                // 1111011_ 101_____
                                                // 11111___ 1001____
                                                // 11111___ 101_____
    constexpr const uint8_t TOO_LARGE_1000 = 1<<6;
                                                // 11110101 1000____
                                                // 1111011_ 1000____
                                                // 11111___ 1000____
    constexpr const uint8_t OVERLONG_4  = 1<<6; // 11110000 1000____

    const simd8<uint8_t> byte_1_high = prev1.shr<4>().lookup_16<uint8_t>(
      // 0_______ ________ <ASCII in byte 1>
      TOO_LONG, TOO_LONG, TOO_LONG, TOO_LONG,
      TOO_LONG, TOO_LONG, TOO_LONG, TOO_LONG,
      // 10______ ________ <continuation in byte 1>
      TWO_CONTS, TWO_CONTS, TWO_CONTS, TWO_CONTS,
      // 1100____ ________ <two byte lead in byte 1>
      TOO_SHORT | OVERLONG_2,
      // 1101____ ________ <two byte lead in byte 1>
      TOO_SHORT,
      // 1110____ ________ <three byte lead in byte 1>
      TOO_SHORT | OVERLONG_3 | SURROGATE,
      // 1111____ ________ <four+ byte lead in byte 1>
      TOO_SHORT | TOO_LARGE | TOO_LARGE_1000 | OVERLONG_4
    );
    constexpr const uint8_t CARRY = TOO_SHORT | TOO_LONG | TWO_CONTS; // These all have ____ in byte 1 .
    const simd8<uint8_t> byte_1_low = (prev1 & 0x0F).lookup_16<uint8_t>(
      // ____0000 ________
      CARRY | OVERLONG_3 | OVERLONG_2 | OVERLONG_4,
      // ____0001 ________
      CARRY | OVERLONG_2,
      // ____001_ ________
      CARRY,
      CARRY,

      // ____0100 ________
      CARRY | TOO_LARGE,
      // ____0101 ________
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      // ____011_ ________
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,

      // ____1___ ________
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      // ____1101 ________
      CARRY | TOO_LARGE | TOO_LARGE_1000 | SURROGATE,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000
    );
    const simd8<uint8_t> byte_2_high = input.shr<4>().lookup_16<uint8_t>(
      // ________ 0_______ <ASCII in byte 2>
      TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,
      TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,

      // ________ 1000____
      TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE_1000 | OVERLONG_4,
      // ________ 1001____
      TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE,
      // ________ 101_____
      TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE  | TOO_LARGE,
      TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE  | TOO_LARGE,

      // ________ 11______
      TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT
    );
    return (byte_1_high & byte_1_low & byte_2_high);
  }
  simdjson_really_inline simd8<uint8_t> check_multibyte_lengths(const simd8<uint8_t> input,
      const simd8<uint8_t> prev_input, const simd8<uint8_t> sc) {
    simd8<uint8_t> prev2 = input.prev<2>(prev_input);
    simd8<uint8_t> prev3 = input.prev<3>(prev_input);
    simd8<uint8_t> must23 = simd8<uint8_t>(must_be_2_3_continuation(prev2, prev3));
    simd8<uint8_t> must23_80 = must23 & uint8_t(0x80);
    return must23_80 ^ sc;
  }

  //
  // Return nonzero if there are incomplete multibyte characters at the end of the block:
  // e.g. if there is a 4-byte character, but it's 3 bytes from the end.
  //
  simdjson_really_inline simd8<uint8_t> is_incomplete(const simd8<uint8_t> input) {
    // If the previous input's last 3 bytes match this, they're too short (they ended at EOF):
    // ... 1111____ 111_____ 11______
    static const uint8_t max_array[32] = {
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 0b11110000u-1, 0b11100000u-1, 0b11000000u-1
    };
    const simd8<uint8_t> max_value(&max_array[sizeof(max_array)-sizeof(simd8<uint8_t>)]);
    return input.gt_bits(max_value);
  }

  struct utf8_checker {
    // If this is nonzero, there has been a UTF-8 error.
    simd8<uint8_t> error;
    // The last input we received
    simd8<uint8_t> prev_input_block;
    // Whether the last input we received was incomplete (used for ASCII fast path)
    simd8<uint8_t> prev_incomplete;

    //
    // Check whether the current bytes are valid UTF-8.
    //
    simdjson_really_inline void check_utf8_bytes(const simd8<uint8_t> input, const simd8<uint8_t> prev_input) {
      // Flip prev1...prev3 so we can easily determine if they are 2+, 3+ or 4+ lead bytes
      // (2, 3, 4-byte leads become large positive numbers instead of small negative numbers)
      simd8<uint8_t> prev1 = input.prev<1>(prev_input);
      simd8<uint8_t> sc = check_special_cases(input, prev1);
      this->error |= check_multibyte_lengths(input, prev_input, sc);
    }

    // The only problem that can happen at EOF is that a multibyte character is too short
    // or a byte value too large in the last bytes: check_special_cases only checks for bytes
    // too large in the first of two bytes.
    simdjson_really_inline void check_eof() {
      // If the previous block had incomplete UTF-8 characters at the end, an ASCII block can't
      // possibly finish them.
      this->error |= this->prev_incomplete;
    }

    simdjson_really_inline void check_next_input(const simd8x64<uint8_t>& input) {
      if(simdjson_likely(is_ascii(input))) {
        this->error |= this->prev_incomplete;
      } else {
        // you might think that a for-loop would work, but under Visual Studio, it is not good enough.
        static_assert((simd8x64<uint8_t>::NUM_CHUNKS == 2) || (simd8x64<uint8_t>::NUM_CHUNKS == 4),
            "We support either two or four chunks per 64-byte block.");
        if(simd8x64<uint8_t>::NUM_CHUNKS == 2) {
          this->check_utf8_bytes(input.chunks[0], this->prev_input_block);
          this->check_utf8_bytes(input.chunks[1], input.chunks[0]);
        } else if(simd8x64<uint8_t>::NUM_CHUNKS == 4) {
          this->check_utf8_bytes(input.chunks[0], this->prev_input_block);
          this->check_utf8_bytes(input.chunks[1], input.chunks[0]);
          this->check_utf8_bytes(input.chunks[2], input.chunks[1]);
          this->check_utf8_bytes(input.chunks[3], input.chunks[2]);
        }
        this->prev_incomplete = is_incomplete(input.chunks[simd8x64<uint8_t>::NUM_CHUNKS-1]);
        this->prev_input_block = input.chunks[simd8x64<uint8_t>::NUM_CHUNKS-1];

      }
    }
    // do not forget to call check_eof!
    simdjson_really_inline error_code errors() {
      return this->error.any_bits_set_anywhere() ? error_code::UTF8_ERROR : error_code::SUCCESS;
    }

  }; // struct utf8_checker
} // namespace utf8_validation

using utf8_validation::utf8_checker;

} // unnamed namespace
} // namespace ppc64
} // namespace simdjson
/* end file src/generic/stage1/utf8_lookup4_algorithm.h */
#include "generic/stage1/json_structural_indexer.h"
/* begin file src/generic/stage1/utf8_validator.h */
namespace simdjson {
namespace ppc64 {
namespace {
namespace stage1 {

/**
 * Validates that the string is actual UTF-8.
 */
template<class checker>
bool generic_validate_utf8(const uint8_t * input, size_t length) {
    checker c{};
    buf_block_reader<64> reader(input, length);
    while (reader.has_full_block()) {
      simd::simd8x64<uint8_t> in(reader.full_block());
      c.check_next_input(in);
      reader.advance();
    }
    uint8_t block[64]{};
    reader.get_remainder(block);
    simd::simd8x64<uint8_t> in(block);
    c.check_next_input(in);
    reader.advance();
    c.check_eof();
    return c.errors() == error_code::SUCCESS;
}

bool generic_validate_utf8(const char * input, size_t length) {
    return generic_validate_utf8<utf8_checker>(reinterpret_cast<const uint8_t *>(input),length);
}

} // namespace stage1
} // unnamed namespace
} // namespace ppc64
} // namespace simdjson
/* end file src/generic/stage1/utf8_validator.h */

//
// Implementation-specific overrides
//
namespace simdjson {
namespace ppc64 {

simdjson_warn_unused bool implementation::validate_utf8(const char *buf, size_t len) const noexcept {
  return ppc64::stage1::generic_validate_utf8(buf,len);
}

} // namespace ppc64
} // namespace simdjson

/* begin file include/simdjson/ppc64/end.h */
/* end file include/simdjson/ppc64/end.h */
/* end file src/ppc64/dom_parser_implementation.cpp */
#endif
#if SIMDJSON_IMPLEMENTATION_WESTMERE
/* begin file src/westmere/dom_parser_implementation.cpp */
/* begin file include/simdjson/westmere/begin.h */
// redefining SIMDJSON_IMPLEMENTATION to "westmere"
// #define SIMDJSON_IMPLEMENTATION westmere
SIMDJSON_TARGET_WESTMERE
/* end file include/simdjson/westmere/begin.h */

//
// Stage 1
//

namespace simdjson {
namespace westmere {
namespace {

using namespace simd;

simdjson_really_inline bool is_ascii(const simd8x64<uint8_t>& input) {
  return input.reduce_or().is_ascii();
}

simdjson_really_inline simd8<bool> must_be_2_3_continuation(const simd8<uint8_t> prev2, const simd8<uint8_t> prev3) {
  simd8<uint8_t> is_third_byte  = prev2.saturating_sub(0b11100000u-1); // Only 111_____ will be > 0
  simd8<uint8_t> is_fourth_byte = prev3.saturating_sub(0b11110000u-1); // Only 1111____ will be > 0
  // Caller requires a bool (all 1's). All values resulting from the subtraction will be <= 64, so signed comparison is fine.
  return simd8<int8_t>(is_third_byte | is_fourth_byte) > int8_t(0);
}

} // unnamed namespace
} // namespace westmere
} // namespace simdjson

/* begin file src/generic/stage1/utf8_lookup4_algorithm.h */
namespace simdjson {
namespace westmere {
namespace {
namespace utf8_validation {

using namespace simd;

  simdjson_really_inline simd8<uint8_t> check_special_cases(const simd8<uint8_t> input, const simd8<uint8_t> prev1) {
// Bit 0 = Too Short (lead byte/ASCII followed by lead byte/ASCII)
// Bit 1 = Too Long (ASCII followed by continuation)
// Bit 2 = Overlong 3-byte
// Bit 4 = Surrogate
// Bit 5 = Overlong 2-byte
// Bit 7 = Two Continuations
    constexpr const uint8_t TOO_SHORT   = 1<<0; // 11______ 0_______
                                                // 11______ 11______
    constexpr const uint8_t TOO_LONG    = 1<<1; // 0_______ 10______
    constexpr const uint8_t OVERLONG_3  = 1<<2; // 11100000 100_____
    constexpr const uint8_t SURROGATE   = 1<<4; // 11101101 101_____
    constexpr const uint8_t OVERLONG_2  = 1<<5; // 1100000_ 10______
    constexpr const uint8_t TWO_CONTS   = 1<<7; // 10______ 10______
    constexpr const uint8_t TOO_LARGE   = 1<<3; // 11110100 1001____
                                                // 11110100 101_____
                                                // 11110101 1001____
                                                // 11110101 101_____
                                                // 1111011_ 1001____
                                                // 1111011_ 101_____
                                                // 11111___ 1001____
                                                // 11111___ 101_____
    constexpr const uint8_t TOO_LARGE_1000 = 1<<6;
                                                // 11110101 1000____
                                                // 1111011_ 1000____
                                                // 11111___ 1000____
    constexpr const uint8_t OVERLONG_4  = 1<<6; // 11110000 1000____

    const simd8<uint8_t> byte_1_high = prev1.shr<4>().lookup_16<uint8_t>(
      // 0_______ ________ <ASCII in byte 1>
      TOO_LONG, TOO_LONG, TOO_LONG, TOO_LONG,
      TOO_LONG, TOO_LONG, TOO_LONG, TOO_LONG,
      // 10______ ________ <continuation in byte 1>
      TWO_CONTS, TWO_CONTS, TWO_CONTS, TWO_CONTS,
      // 1100____ ________ <two byte lead in byte 1>
      TOO_SHORT | OVERLONG_2,
      // 1101____ ________ <two byte lead in byte 1>
      TOO_SHORT,
      // 1110____ ________ <three byte lead in byte 1>
      TOO_SHORT | OVERLONG_3 | SURROGATE,
      // 1111____ ________ <four+ byte lead in byte 1>
      TOO_SHORT | TOO_LARGE | TOO_LARGE_1000 | OVERLONG_4
    );
    constexpr const uint8_t CARRY = TOO_SHORT | TOO_LONG | TWO_CONTS; // These all have ____ in byte 1 .
    const simd8<uint8_t> byte_1_low = (prev1 & 0x0F).lookup_16<uint8_t>(
      // ____0000 ________
      CARRY | OVERLONG_3 | OVERLONG_2 | OVERLONG_4,
      // ____0001 ________
      CARRY | OVERLONG_2,
      // ____001_ ________
      CARRY,
      CARRY,

      // ____0100 ________
      CARRY | TOO_LARGE,
      // ____0101 ________
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      // ____011_ ________
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,

      // ____1___ ________
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      // ____1101 ________
      CARRY | TOO_LARGE | TOO_LARGE_1000 | SURROGATE,
      CARRY | TOO_LARGE | TOO_LARGE_1000,
      CARRY | TOO_LARGE | TOO_LARGE_1000
    );
    const simd8<uint8_t> byte_2_high = input.shr<4>().lookup_16<uint8_t>(
      // ________ 0_______ <ASCII in byte 2>
      TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,
      TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT,

      // ________ 1000____
      TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE_1000 | OVERLONG_4,
      // ________ 1001____
      TOO_LONG | OVERLONG_2 | TWO_CONTS | OVERLONG_3 | TOO_LARGE,
      // ________ 101_____
      TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE  | TOO_LARGE,
      TOO_LONG | OVERLONG_2 | TWO_CONTS | SURROGATE  | TOO_LARGE,

      // ________ 11______
      TOO_SHORT, TOO_SHORT, TOO_SHORT, TOO_SHORT
    );
    return (byte_1_high & byte_1_low & byte_2_high);
  }
  simdjson_really_inline simd8<uint8_t> check_multibyte_lengths(const simd8<uint8_t> input,
      const simd8<uint8_t> prev_input, const simd8<uint8_t> sc) {
    simd8<uint8_t> prev2 = input.prev<2>(prev_input);
    simd8<uint8_t> prev3 = input.prev<3>(prev_input);
    simd8<uint8_t> must23 = simd8<uint8_t>(must_be_2_3_continuation(prev2, prev3));
    simd8<uint8_t> must23_80 = must23 & uint8_t(0x80);
    return must23_80 ^ sc;
  }

  //
  // Return nonzero if there are incomplete multibyte characters at the end of the block:
  // e.g. if there is a 4-byte character, but it's 3 bytes from the end.
  //
  simdjson_really_inline simd8<uint8_t> is_incomplete(const simd8<uint8_t> input) {
    // If the previous input's last 3 bytes match this, they're too short (they ended at EOF):
    // ... 1111____ 111_____ 11______
    static const uint8_t max_array[32] = {
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 255, 255, 255,
      255, 255, 255, 255, 255, 0b11110000u-1, 0b11100000u-1, 0b11000000u-1
    };
    const simd8<uint8_t> max_value(&max_array[sizeof(max_array)-sizeof(simd8<uint8_t>)]);
    return input.gt_bits(max_value);
  }

  struct utf8_checker {
    // If this is nonzero, there has been a UTF-8 error.
    simd8<uint8_t> error;
    // The last input we received
    simd8<uint8_t> prev_input_block;
    // Whether the last input we received was incomplete (used for ASCII fast path)
    simd8<uint8_t> prev_incomplete;

    //
    // Check whether the current bytes are valid UTF-8.
    //
    simdjson_really_inline void check_utf8_bytes(const simd8<uint8_t> input, const simd8<uint8_t> prev_input) {
      // Flip prev1...prev3 so we can easily determine if they are 2+, 3+ or 4+ lead bytes
      // (2, 3, 4-byte leads become large positive numbers instead of small negative numbers)
      simd8<uint8_t> prev1 = input.prev<1>(prev_input);
      simd8<uint8_t> sc = check_special_cases(input, prev1);
      this->error |= check_multibyte_lengths(input, prev_input, sc);
    }

    // The only problem that can happen at EOF is that a multibyte character is too short
    // or a byte value too large in the last bytes: check_special_cases only checks for bytes
    // too large in the first of two bytes.
    simdjson_really_inline void check_eof() {
      // If the previous block had incomplete UTF-8 characters at the end, an ASCII block can't
      // possibly finish them.
      this->error |= this->prev_incomplete;
    }

    simdjson_really_inline void check_next_input(const simd8x64<uint8_t>& input) {
      if(simdjson_likely(is_ascii(input))) {
        this->error |= this->prev_incomplete;
      } else {
        // you might think that a for-loop would work, but under Visual Studio, it is not good enough.
        static_assert((simd8x64<uint8_t>::NUM_CHUNKS == 2) || (simd8x64<uint8_t>::NUM_CHUNKS == 4),
            "We support either two or four chunks per 64-byte block.");
        if(simd8x64<uint8_t>::NUM_CHUNKS == 2) {
          this->check_utf8_bytes(input.chunks[0], this->prev_input_block);
          this->check_utf8_bytes(input.chunks[1], input.chunks[0]);
        } else if(simd8x64<uint8_t>::NUM_CHUNKS == 4) {
          this->check_utf8_bytes(input.chunks[0], this->prev_input_block);
          this->check_utf8_bytes(input.chunks[1], input.chunks[0]);
          this->check_utf8_bytes(input.chunks[2], input.chunks[1]);
          this->check_utf8_bytes(input.chunks[3], input.chunks[2]);
        }
        this->prev_incomplete = is_incomplete(input.chunks[simd8x64<uint8_t>::NUM_CHUNKS-1]);
        this->prev_input_block = input.chunks[simd8x64<uint8_t>::NUM_CHUNKS-1];

      }
    }
    // do not forget to call check_eof!
    simdjson_really_inline error_code errors() {
      return this->error.any_bits_set_anywhere() ? error_code::UTF8_ERROR : error_code::SUCCESS;
    }

  }; // struct utf8_checker
} // namespace utf8_validation

using utf8_validation::utf8_checker;

} // unnamed namespace
} // namespace westmere
} // namespace simdjson
/* end file src/generic/stage1/utf8_lookup4_algorithm.h */
/* begin file src/generic/stage1/buf_block_reader.h */
#include <cstring>

namespace simdjson {
namespace westmere {
namespace {

// Walks through a buffer in block-sized increments, loading the last part with spaces
template<size_t STEP_SIZE>
struct buf_block_reader {
public:
  simdjson_really_inline buf_block_reader(const uint8_t *_buf, size_t _len);
  simdjson_really_inline size_t block_index();
  simdjson_really_inline bool has_full_block() const;
  simdjson_really_inline const uint8_t *full_block() const;
  /**
   * Get the last block, padded with spaces.
   *
   * There will always be a last block, with at least 1 byte, unless len == 0 (in which case this
   * function fills the buffer with spaces and returns 0. In particular, if len == STEP_SIZE there
   * will be 0 full_blocks and 1 remainder block with STEP_SIZE bytes and no spaces for padding.
   *
   * @return the number of effective characters in the last block.
   */
  simdjson_really_inline size_t get_remainder(uint8_t *dst) const;
  simdjson_really_inline void advance();
private:
  const uint8_t *buf;
  const size_t len;
  const size_t lenminusstep;
  size_t idx;
};

// Routines to print masks and text for debugging bitmask operations
simdjson_unused static char * format_input_text_64(const uint8_t *text) {
  static char buf[sizeof(simd8x64<uint8_t>) + 1];
  for (size_t i=0; i<sizeof(simd8x64<uint8_t>); i++) {
    buf[i] = int8_t(text[i]) < ' ' ? '_' : int8_t(text[i]);
  }
  buf[sizeof(simd8x64<uint8_t>)] = '\0';
  return buf;
}

// Routines to print masks and text for debugging bitmask operations
simdjson_unused static char * format_input_text(const simd8x64<uint8_t>& in) {
  static char buf[sizeof(simd8x64<uint8_t>) + 1];
  in.store(reinterpret_cast<uint8_t*>(buf));
  for (size_t i=0; i<sizeof(simd8x64<uint8_t>); i++) {
    if (buf[i] < ' ') { buf[i] = '_'; }
  }
  buf[sizeof(simd8x64<uint8_t>)] = '\0';
  return buf;
}

simdjson_unused static char * format_mask(uint64_t mask) {
  static char buf[sizeof(simd8x64<uint8_t>) + 1];
  for (size_t i=0; i<64; i++) {
    buf[i] = (mask & (size_t(1) << i)) ? 'X' : ' ';
  }
  buf[64] = '\0';
  return buf;
}

template<size_t STEP_SIZE>
simdjson_really_inline buf_block_reader<STEP_SIZE>::buf_block_reader(const uint8_t *_buf, size_t _len) : buf{_buf}, len{_len}, lenminusstep{len < STEP_SIZE ? 0 : len - STEP_SIZE}, idx{0} {}

template<size_t STEP_SIZE>
simdjson_really_inline size_t buf_block_reader<STEP_SIZE>::block_index() { return idx; }

template<size_t STEP_SIZE>
simdjson_really_inline bool buf_block_reader<STEP_SIZE>::has_full_block() const {
  return idx < lenminusstep;
}

template<size_t STEP_SIZE>
simdjson_really_inline const uint8_t *buf_block_reader<STEP_SIZE>::full_block() const {
  return &buf[idx];
}

template<size_t STEP_SIZE>
simdjson_really_inline size_t buf_block_reader<STEP_SIZE>::get_remainder(uint8_t *dst) const {
  if(len == idx) { return 0; } // memcpy(dst, null, 0) will trigger an error with some sanitizers
  std::memset(dst, 0x20, STEP_SIZE); // std::memset STEP_SIZE because it's more efficient to write out 8 or 16 bytes at once.
  std::memcpy(dst, buf + idx, len - idx);
  return len - idx;
}

template<size_t STEP_SIZE>
simdjson_really_inline void buf_block_reader<STEP_SIZE>::advance() {
  idx += STEP_SIZE;
}

} // unnamed namespace
} // namespace westmere
} // namespace simdjson
/* end file src/generic/stage1/buf_block_reader.h */
/* begin file src/generic/stage1/utf8_validator.h */
namespace simdjson {
namespace westmere {
namespace {
namespace stage1 {

/**
 * Validates that the string is actual UTF-8.
 */
template<class checker>
bool generic_validate_utf8(const uint8_t * input, size_t length) {
    checker c{};
    buf_block_reader<64> reader(input, length);
    while (reader.has_full_block()) {
      simd::simd8x64<uint8_t> in(reader.full_block());
      c.check_next_input(in);
      reader.advance();
    }
    uint8_t block[64]{};
    reader.get_remainder(block);
    simd::simd8x64<uint8_t> in(block);
    c.check_next_input(in);
    reader.advance();
    c.check_eof();
    return c.errors() == error_code::SUCCESS;
}

bool generic_validate_utf8(const char * input, size_t length) {
    return generic_validate_utf8<utf8_checker>(reinterpret_cast<const uint8_t *>(input),length);
}

} // namespace stage1
} // unnamed namespace
} // namespace westmere
} // namespace simdjson
/* end file src/generic/stage1/utf8_validator.h */

//
// Implementation-specific overrides
//
namespace simdjson {
namespace westmere {

simdjson_warn_unused bool implementation::validate_utf8(const char *buf, size_t len) const noexcept
{
  return westmere::stage1::generic_validate_utf8(buf,len);
}

} // namespace westmere
} // namespace simdjson

/* begin file include/simdjson/westmere/end.h */
SIMDJSON_UNTARGET_WESTMERE
/* end file include/simdjson/westmere/end.h */
/* end file src/westmere/dom_parser_implementation.cpp */
#endif

SIMDJSON_POP_DISABLE_WARNINGS
/* end file src/simdjson.cpp */
