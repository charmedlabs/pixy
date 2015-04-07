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

package wchar_h is

   subtype uu_mbstate_t_uu_wchb_array is Interfaces.C.char_array (0 .. 3);
   type anon_2 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_wch : aliased unsigned;  -- /usr/include/wchar.h:88
         when others =>
            uu_wchb : aliased uu_mbstate_t_uu_wchb_array;  -- /usr/include/wchar.h:92
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_2);
   pragma Unchecked_Union (anon_2);
   type uu_mbstate_t is record
      uu_count : aliased int;  -- /usr/include/wchar.h:84
      uu_value : anon_2;  -- /usr/include/wchar.h:93
   end record;
   pragma Convention (C_Pass_By_Copy, uu_mbstate_t);  -- /usr/include/wchar.h:94

   --  skipped anonymous struct anon_1

end wchar_h;
