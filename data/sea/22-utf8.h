#include "21-time.h"

// Check if the string is a valid utf8 string.
static ibool_t istring_is_valid(const istring_t str) {
  const char *s = (const istring_t) str;

  while ('\0' != *s) {
    if (0xf0 == (0xf8 & *s)) {
      // ensure each of the 3 following bytes in this 4-byte
      // utf8 codepoint began with 0b10xxxxxx
      if ((0x80 != (0xc0 & s[1])) || (0x80 != (0xc0 & s[2])) ||
          (0x80 != (0xc0 & s[3]))) {
        return ifalse;
      }

      // ensure that our utf8 codepoint ended after 4 bytes
      if (0x80 == (0xc0 & s[4])) {
        return ifalse;
      }

      // ensure that the top 5 bits of this 4-byte utf8
      // codepoint were not 0, as then we could have used
      // one of the smaller encodings
      if ((0 == (0x07 & s[0])) && (0 == (0x30 & s[1]))) {
        return ifalse;
      }

      // 4-byte utf8 code point (began with 0b11110xxx)
      s += 4;
    } else if (0xe0 == (0xf0 & *s)) {
      // ensure each of the 2 following bytes in this 3-byte
      // utf8 codepoint began with 0b10xxxxxx
      if ((0x80 != (0xc0 & s[1])) || (0x80 != (0xc0 & s[2]))) {
        return ifalse;
      }

      // ensure that our utf8 codepoint ended after 3 bytes
      if (0x80 == (0xc0 & s[3])) {
        return ifalse;
      }

      // ensure that the top 5 bits of this 3-byte utf8
      // codepoint were not 0, as then we could have used
      // one of the smaller encodings
      if ((0 == (0x0f & s[0])) && (0 == (0x20 & s[1]))) {
        return ifalse;
      }

      // 3-byte utf8 code point (began with 0b1110xxxx)
      s += 3;
    } else if (0xc0 == (0xe0 & *s)) {
      // ensure the 1 following byte in this 2-byte
      // utf8 codepoint began with 0b10xxxxxx
      if (0x80 != (0xc0 & s[1])) {
        return ifalse;
      }

      // ensure that our utf8 codepoint ended after 2 bytes
      if (0x80 == (0xc0 & s[2])) {
        return ifalse;
      }

      // ensure that the top 4 bits of this 2-byte utf8
      // codepoint were not 0, as then we could have used
      // one of the smaller encodings
      if (0 == (0x1e & s[0])) {
        return ifalse;
      }

      // 2-byte utf8 code point (began with 0b110xxxxx)
      s += 2;
    } else if (0x00 == (0x80 & *s)) {
      // 1-byte ascii (began with 0b0xxxxxxx)
      s += 1;
    } else {
      // we have an invalid 0b1xxxxxxx utf8 code point entry
      return ifalse;
    }
  }

  return itrue;
}

// Calculate the length of a string. This counts
// code points, so modifiers *will* be counted. as
// characters.
static iint_t istring_length(const istring_t str) {
  const unsigned char *s = (const unsigned char *)str;
  iint_t length = 0;

  while ('\0' != *s) {
    if (0xf0 == (0xf8 & *s)) {
      // 4-byte utf8 code point (began with 0b11110xxx)
      s += 4;
    } else if (0xe0 == (0xf0 & *s)) {
      // 3-byte utf8 code point (began with 0b1110xxxx)
      s += 3;
    } else if (0xc0 == (0xe0 & *s)) {
      // 2-byte utf8 code point (began with 0b110xxxxx)
      s += 2;
    } else { // if (0x00 == (0x80 & *s)) {
      // 1-byte ascii (began with 0b0xxxxxxx)
      s += 1;
    }

    // no matter the bytes we marched s forward by, it was
    // only 1 utf8 codepoint
    length++;
  }

  return length;
}

// Sets out_codepoint to the next utf8 codepoint in str,
// and returns the address of the utf8 codepoint after the current one in str.
static istring_t utf8codepoint(const istring_t str,
                    int32_t * out_codepoint) {
  const char *s = (const char *)str;

  if (0xf0 == (0xf8 & s[0])) {
    // 4 byte utf8 codepoint
    *out_codepoint = ((0x07 & s[0]) << 18) | ((0x3f & s[1]) << 12) |
                     ((0x3f & s[2]) << 6) | (0x3f & s[3]);
    s += 4;
  } else if (0xe0 == (0xf0 & s[0])) {
    // 3 byte utf8 codepoint
    *out_codepoint =
        ((0x0f & s[0]) << 12) | ((0x3f & s[1]) << 6) | (0x3f & s[2]);
    s += 3;
  } else if (0xc0 == (0xe0 & s[0])) {
    // 2 byte utf8 codepoint
    *out_codepoint = ((0x1f & s[0]) << 6) | (0x3f & s[1]);
    s += 2;
  } else {
    // 1 byte utf8 codepoint otherwise
    *out_codepoint = s[0];
    s += 1;
  }

  return s;
}

// Return the size of a codepoint in bytes
static size_t utf8codepointsize(int32_t chr) {
  if (0 == ((int32_t)0xffffff80 & chr)) {
    return 1;
  } else if (0 == ((int32_t)0xfffff800 & chr)) {
    return 2;
  } else if (0 == ((int32_t)0xffff0000 & chr)) {
    return 3;
  } else { // if (0 == ((int)0xffe00000 & chr)) {
    return 4;
  }
}

static istring_t utf8catcodepoint(const istring_t str, int32_t chr, size_t n) {
  char *s = (char *)str;

  if (0 == ((int32_t)0xffffff80 & chr)) {
    // 1-byte/7-bit ascii
    // (0b0xxxxxxx)
    if (n < 1) {
      return NULL;
    }
    s[0] = (char)chr;
    s += 1;
  } else if (0 == ((int32_t)0xfffff800 & chr)) {
    // 2-byte/11-bit utf8 code point
    // (0b110xxxxx 0b10xxxxxx)
    if (n < 2) {
      return NULL;
    }
    s[0] = 0xc0 | (char)(chr >> 6);
    s[1] = 0x80 | (char)(chr & 0x3f);
    s += 2;
  } else if (0 == ((int32_t)0xffff0000 & chr)) {
    // 3-byte/16-bit utf8 code point
    // (0b1110xxxx 0b10xxxxxx 0b10xxxxxx)
    if (n < 3) {
      return NULL;
    }
    s[0] = 0xe0 | (char)(chr >> 12);
    s[1] = 0x80 | (char)((chr >> 6) & 0x3f);
    s[2] = 0x80 | (char)(chr & 0x3f);
    s += 3;
  } else { // if (0 == ((int)0xffe00000 & chr)) {
    // 4-byte/21-bit utf8 code point
    // (0b11110xxx 0b10xxxxxx 0b10xxxxxx 0b10xxxxxx)
    if (n < 4) {
      return NULL;
    }
    s[0] = 0xf0 | (char)(chr >> 18);
    s[1] = 0x80 | (char)((chr >> 12) & 0x3f);
    s[2] = 0x80 | (char)((chr >> 6) & 0x3f);
    s[3] = 0x80 | (char)(chr & 0x3f);
    s += 4;
  }

  return s;
}

// Make a codepoint lower case if possible.
static int32_t utf8lwrcodepoint(int32_t cp) {
  if (((0x0041 <= cp) && (0x005a >= cp)) ||
      ((0x00c0 <= cp) && (0x00d6 >= cp)) ||
      ((0x00d8 <= cp) && (0x00de >= cp)) ||
      ((0x0391 <= cp) && (0x03a1 >= cp)) ||
      ((0x03a3 <= cp) && (0x03ab >= cp))) {
    cp += 32;
  } else if (((0x0100 <= cp) && (0x012f >= cp)) ||
             ((0x0132 <= cp) && (0x0137 >= cp)) ||
             ((0x014a <= cp) && (0x0177 >= cp)) ||
             ((0x0182 <= cp) && (0x0185 >= cp)) ||
             ((0x01a0 <= cp) && (0x01a5 >= cp)) ||
             ((0x01de <= cp) && (0x01ef >= cp)) ||
             ((0x01f8 <= cp) && (0x021f >= cp)) ||
             ((0x0222 <= cp) && (0x0233 >= cp)) ||
             ((0x0246 <= cp) && (0x024f >= cp)) ||
             ((0x03d8 <= cp) && (0x03ef >= cp))) {
    cp |= 0x1;
  } else if (((0x0139 <= cp) && (0x0148 >= cp)) ||
             ((0x0179 <= cp) && (0x017e >= cp)) ||
             ((0x01af <= cp) && (0x01b0 >= cp)) ||
             ((0x01b3 <= cp) && (0x01b6 >= cp)) ||
             ((0x01cd <= cp) && (0x01dc >= cp))) {
    cp += 1;
    cp &= ~0x1;
  } else {
    switch (cp) {
    default: break;
    case 0x0178: cp = 0x00ff; break;
    case 0x0243: cp = 0x0180; break;
    case 0x018e: cp = 0x01dd; break;
    case 0x023d: cp = 0x019a; break;
    case 0x0220: cp = 0x019e; break;
    case 0x01b7: cp = 0x0292; break;
    case 0x01c4: cp = 0x01c6; break;
    case 0x01c7: cp = 0x01c9; break;
    case 0x01ca: cp = 0x01cc; break;
    case 0x01f1: cp = 0x01f3; break;
    case 0x01f7: cp = 0x01bf; break;
    case 0x0187: cp = 0x0188; break;
    case 0x018b: cp = 0x018c; break;
    case 0x0191: cp = 0x0192; break;
    case 0x0198: cp = 0x0199; break;
    case 0x01a7: cp = 0x01a8; break;
    case 0x01ac: cp = 0x01ad; break;
    case 0x01af: cp = 0x01b0; break;
    case 0x01b8: cp = 0x01b9; break;
    case 0x01bc: cp = 0x01bd; break;
    case 0x01f4: cp = 0x01f5; break;
    case 0x023b: cp = 0x023c; break;
    case 0x0241: cp = 0x0242; break;
    case 0x03fd: cp = 0x037b; break;
    case 0x03fe: cp = 0x037c; break;
    case 0x03ff: cp = 0x037d; break;
    case 0x037f: cp = 0x03f3; break;
    case 0x0386: cp = 0x03ac; break;
    case 0x0388: cp = 0x03ad; break;
    case 0x0389: cp = 0x03ae; break;
    case 0x038a: cp = 0x03af; break;
    case 0x038c: cp = 0x03cc; break;
    case 0x038e: cp = 0x03cd; break;
    case 0x038f: cp = 0x03ce; break;
    case 0x0370: cp = 0x0371; break;
    case 0x0372: cp = 0x0373; break;
    case 0x0376: cp = 0x0377; break;
    case 0x03f4: cp = 0x03d1; break;
    case 0x03cf: cp = 0x03d7; break;
    case 0x03f9: cp = 0x03f2; break;
    case 0x03f7: cp = 0x03f8; break;
    case 0x03fa: cp = 0x03fb; break;
    };
  }

  return cp;
}


static int32_t utf8uprcodepoint(int32_t cp) {
  if (((0x0061 <= cp) && (0x007a >= cp)) ||
      ((0x00e0 <= cp) && (0x00f6 >= cp)) ||
      ((0x00f8 <= cp) && (0x00fe >= cp)) ||
      ((0x03b1 <= cp) && (0x03c1 >= cp)) ||
      ((0x03c3 <= cp) && (0x03cb >= cp))) {
    cp -= 32;
  } else if (((0x0100 <= cp) && (0x012f >= cp)) ||
             ((0x0132 <= cp) && (0x0137 >= cp)) ||
             ((0x014a <= cp) && (0x0177 >= cp)) ||
             ((0x0182 <= cp) && (0x0185 >= cp)) ||
             ((0x01a0 <= cp) && (0x01a5 >= cp)) ||
             ((0x01de <= cp) && (0x01ef >= cp)) ||
             ((0x01f8 <= cp) && (0x021f >= cp)) ||
             ((0x0222 <= cp) && (0x0233 >= cp)) ||
             ((0x0246 <= cp) && (0x024f >= cp)) ||
             ((0x03d8 <= cp) && (0x03ef >= cp))) {
    cp &= ~0x1;
  } else if (((0x0139 <= cp) && (0x0148 >= cp)) ||
             ((0x0179 <= cp) && (0x017e >= cp)) ||
             ((0x01af <= cp) && (0x01b0 >= cp)) ||
             ((0x01b3 <= cp) && (0x01b6 >= cp)) ||
             ((0x01cd <= cp) && (0x01dc >= cp))) {
    cp -= 1;
    cp |= 0x1;
  } else {
    switch (cp) {
    default: break;
    case 0x00ff: cp = 0x0178; break;
    case 0x0180: cp = 0x0243; break;
    case 0x01dd: cp = 0x018e; break;
    case 0x019a: cp = 0x023d; break;
    case 0x019e: cp = 0x0220; break;
    case 0x0292: cp = 0x01b7; break;
    case 0x01c6: cp = 0x01c4; break;
    case 0x01c9: cp = 0x01c7; break;
    case 0x01cc: cp = 0x01ca; break;
    case 0x01f3: cp = 0x01f1; break;
    case 0x01bf: cp = 0x01f7; break;
    case 0x0188: cp = 0x0187; break;
    case 0x018c: cp = 0x018b; break;
    case 0x0192: cp = 0x0191; break;
    case 0x0199: cp = 0x0198; break;
    case 0x01a8: cp = 0x01a7; break;
    case 0x01ad: cp = 0x01ac; break;
    case 0x01b0: cp = 0x01af; break;
    case 0x01b9: cp = 0x01b8; break;
    case 0x01bd: cp = 0x01bc; break;
    case 0x01f5: cp = 0x01f4; break;
    case 0x023c: cp = 0x023b; break;
    case 0x0242: cp = 0x0241; break;
    case 0x037b: cp = 0x03fd; break;
    case 0x037c: cp = 0x03fe; break;
    case 0x037d: cp = 0x03ff; break;
    case 0x03f3: cp = 0x037f; break;
    case 0x03ac: cp = 0x0386; break;
    case 0x03ad: cp = 0x0388; break;
    case 0x03ae: cp = 0x0389; break;
    case 0x03af: cp = 0x038a; break;
    case 0x03cc: cp = 0x038c; break;
    case 0x03cd: cp = 0x038e; break;
    case 0x03ce: cp = 0x038f; break;
    case 0x0371: cp = 0x0370; break;
    case 0x0373: cp = 0x0372; break;
    case 0x0377: cp = 0x0376; break;
    case 0x03d1: cp = 0x03f4; break;
    case 0x03d7: cp = 0x03cf; break;
    case 0x03f2: cp = 0x03f9; break;
    case 0x03f8: cp = 0x03f7; break;
    case 0x03fb: cp = 0x03fa; break;
    };
  }

  return cp;
}

// Create a new icicle string with the case requested.
static istring_t INLINE istring_to_case(anemone_mempool_t *into, int32_t (*change_codepoint)(int32_t), istring_t val) {
  // String pointers
  istring_t ret, watch, next;
  // String pointers use in the slow path (see below).
  istring_t slowret, slowwatch;

  // pointer to the write location
  char* work;

  // current code point
  int32_t cp;

  // Size variables.
  size_t val_size, written, remaining;

  val_size = strlen(val);
  ret      = (istring_t) anemone_mempool_alloc (into, val_size + 1);
  written  = 0;
  work     = (char*) ret;
  watch    = val;
  next     = utf8codepoint(watch, &cp);

  // Loop through the input string's code
  // points, converting them to the required
  // case, and writing them to the output.
  while (cp != 0) {
    const int32_t lwr_cp = change_codepoint(cp);
    const size_t size = utf8codepointsize(lwr_cp);

    // Go to the slow path if we would
    // otherwise overrun the buffer.
    if (written + size > val_size)
      goto slowpath;

    // Write the code point and get a
    // new work pointer.
    work  = (char*) utf8catcodepoint(work, lwr_cp, size);
    watch = next;
    next  = utf8codepoint(watch, &cp);
    written += size;
  }

  *work = (char) 0;
  return ret;

// This should be very unusual, and only happen
// when we have codepoints which have a case
// version longer than the input. Even then, it
// should only happen for the last few characters,
// so the extra work should be minimal.
//
// We measure the size of the remaining buffer
// we need to allocate, allocate it, then run
// over the input again copying the lower case
// version into it.
//
// Unfortunately, we will be stuck with an unused
// buffer allocated.
slowpath:

  remaining = 0;
  // We need a variable to track the input (like watch)
  // but will need to reset if after measurement, so
  // make a new one.
  slowwatch = watch;

  // Measurement pass.
  while (cp != 0) {
    const int32_t lwr_cp = change_codepoint(cp);
    const size_t size    = utf8codepointsize(lwr_cp);

    remaining += size;
    slowwatch  = next;
    next       = utf8codepoint(slowwatch, &cp);
  }

  // Allocate a new string to fit the buffer and copy
  // what we've already transformed into it.
  // We'll just have to live with the extra allocation.
  slowret = (istring_t) anemone_mempool_alloc(into, written + remaining + 1);
  memcpy ((char *) slowret, ret, written);

  // Reset our pointers for the start of the loop.
  slowwatch = watch;
  next      = utf8codepoint(slowwatch, &cp);
  work      = (char*) slowret + written;

  // Write pass.
  while (cp != 0) {
    const int32_t lwr_cp = utf8lwrcodepoint(cp);
    const size_t size = utf8codepointsize(lwr_cp);

    work      = (char*) utf8catcodepoint(work, lwr_cp, size);
    slowwatch = next;
    next      = utf8codepoint(slowwatch, &cp);
  }

  return slowret;
}

static istring_t INLINE istring_to_lower(anemone_mempool_t *into, istring_t val) {
  return istring_to_case(into, &utf8lwrcodepoint, val);
}

static istring_t INLINE istring_to_upper(anemone_mempool_t *into, istring_t val) {
  return istring_to_case(into, &utf8uprcodepoint , val);
}
