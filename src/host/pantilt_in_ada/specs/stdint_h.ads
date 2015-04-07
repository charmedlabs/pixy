--
--  Copyright (c) 2015, John Leimon <jleimon@gmail.com>
--
--  Permission to use, copy, modify, and/or distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above copyright
--  notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD
--  TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN
--  NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
--  CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
--  PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
--  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package stdint_h is

   --  unsupported macro: INT8_MIN (-128)
   --  unsupported macro: INT16_MIN (-32767-1)
   --  unsupported macro: INT32_MIN (-2147483647-1)
   --  unsupported macro: INT64_MIN (-__INT64_C(9223372036854775807)-1)
   --  unsupported macro: INT8_MAX (127)
   --  unsupported macro: INT16_MAX (32767)
   --  unsupported macro: INT32_MAX (2147483647)
   --  unsupported macro: INT64_MAX (__INT64_C(9223372036854775807))
   --  unsupported macro: UINT8_MAX (255)
   --  unsupported macro: UINT16_MAX (65535)
   --  unsupported macro: UINT32_MAX (4294967295U)
   --  unsupported macro: UINT64_MAX (__UINT64_C(18446744073709551615))
   --  unsupported macro: INT_LEAST8_MIN (-128)
   --  unsupported macro: INT_LEAST16_MIN (-32767-1)
   --  unsupported macro: INT_LEAST32_MIN (-2147483647-1)
   --  unsupported macro: INT_LEAST64_MIN (-__INT64_C(9223372036854775807)-1)
   --  unsupported macro: INT_LEAST8_MAX (127)
   --  unsupported macro: INT_LEAST16_MAX (32767)
   --  unsupported macro: INT_LEAST32_MAX (2147483647)
   --  unsupported macro: INT_LEAST64_MAX (__INT64_C(9223372036854775807))
   --  unsupported macro: UINT_LEAST8_MAX (255)
   --  unsupported macro: UINT_LEAST16_MAX (65535)
   --  unsupported macro: UINT_LEAST32_MAX (4294967295U)
   --  unsupported macro: UINT_LEAST64_MAX (__UINT64_C(18446744073709551615))
   --  unsupported macro: INT_FAST8_MIN (-128)
   --  unsupported macro: INT_FAST16_MIN (-9223372036854775807L-1)
   --  unsupported macro: INT_FAST32_MIN (-9223372036854775807L-1)
   --  unsupported macro: INT_FAST64_MIN (-__INT64_C(9223372036854775807)-1)
   --  unsupported macro: INT_FAST8_MAX (127)
   --  unsupported macro: INT_FAST16_MAX (9223372036854775807L)
   --  unsupported macro: INT_FAST32_MAX (9223372036854775807L)
   --  unsupported macro: INT_FAST64_MAX (__INT64_C(9223372036854775807))
   --  unsupported macro: UINT_FAST8_MAX (255)
   --  unsupported macro: UINT_FAST16_MAX (18446744073709551615UL)
   --  unsupported macro: UINT_FAST32_MAX (18446744073709551615UL)
   --  unsupported macro: UINT_FAST64_MAX (__UINT64_C(18446744073709551615))
   --  unsupported macro: INTPTR_MIN (-9223372036854775807L-1)
   --  unsupported macro: INTPTR_MAX (9223372036854775807L)
   --  unsupported macro: UINTPTR_MAX (18446744073709551615UL)
   --  unsupported macro: INTMAX_MIN (-__INT64_C(9223372036854775807)-1)
   --  unsupported macro: INTMAX_MAX (__INT64_C(9223372036854775807))
   --  unsupported macro: UINTMAX_MAX (__UINT64_C(18446744073709551615))
   --  unsupported macro: PTRDIFF_MIN (-9223372036854775807L-1)
   --  unsupported macro: PTRDIFF_MAX (9223372036854775807L)
   --  unsupported macro: SIG_ATOMIC_MIN (-2147483647-1)
   --  unsupported macro: SIG_ATOMIC_MAX (2147483647)
   --  unsupported macro: SIZE_MAX (18446744073709551615UL)
   --  unsupported macro: WCHAR_MIN __WCHAR_MIN
   --  unsupported macro: WCHAR_MAX __WCHAR_MAX
   --  unsupported macro: WINT_MIN (0u)
   --  unsupported macro: WINT_MAX (4294967295u)
   --  arg-macro: procedure INT8_C (c)
   --    c
   --  arg-macro: procedure INT16_C (c)
   --    c
   --  arg-macro: procedure INT32_C (c)
   --    c
   --  unsupported macro: INT64_C(c) c ## L
   --  arg-macro: procedure UINT8_C (c)
   --    c
   --  arg-macro: procedure UINT16_C (c)
   --    c
   --  unsupported macro: UINT32_C(c) c ## U
   --  unsupported macro: UINT64_C(c) c ## UL
   --  unsupported macro: INTMAX_C(c) c ## L
   --  unsupported macro: UINTMAX_C(c) c ## UL
   subtype int8_t is signed_char;  -- /usr/include/stdint.h:36

   subtype int16_t is short;  -- /usr/include/stdint.h:37

   subtype int32_t is int;  -- /usr/include/stdint.h:38

   subtype int64_t is long;  -- /usr/include/stdint.h:40

   subtype uint8_t is unsigned_char;  -- /usr/include/stdint.h:48

   subtype uint16_t is unsigned_short;  -- /usr/include/stdint.h:49

   subtype uint32_t is unsigned;  -- /usr/include/stdint.h:51

   subtype uint64_t is unsigned_long;  -- /usr/include/stdint.h:55

   subtype int_least8_t is signed_char;  -- /usr/include/stdint.h:65

   subtype int_least16_t is short;  -- /usr/include/stdint.h:66

   subtype int_least32_t is int;  -- /usr/include/stdint.h:67

   subtype int_least64_t is long;  -- /usr/include/stdint.h:69

   subtype uint_least8_t is unsigned_char;  -- /usr/include/stdint.h:76

   subtype uint_least16_t is unsigned_short;  -- /usr/include/stdint.h:77

   subtype uint_least32_t is unsigned;  -- /usr/include/stdint.h:78

   subtype uint_least64_t is unsigned_long;  -- /usr/include/stdint.h:80

   subtype int_fast8_t is signed_char;  -- /usr/include/stdint.h:90

   subtype int_fast16_t is long;  -- /usr/include/stdint.h:92

   subtype int_fast32_t is long;  -- /usr/include/stdint.h:93

   subtype int_fast64_t is long;  -- /usr/include/stdint.h:94

   subtype uint_fast8_t is unsigned_char;  -- /usr/include/stdint.h:103

   subtype uint_fast16_t is unsigned_long;  -- /usr/include/stdint.h:105

   subtype uint_fast32_t is unsigned_long;  -- /usr/include/stdint.h:106

   subtype uint_fast64_t is unsigned_long;  -- /usr/include/stdint.h:107

   subtype intptr_t is long;  -- /usr/include/stdint.h:119

   subtype uintptr_t is unsigned_long;  -- /usr/include/stdint.h:122

   subtype intmax_t is long;  -- /usr/include/stdint.h:134

   subtype uintmax_t is unsigned_long;  -- /usr/include/stdint.h:135

end stdint_h;
