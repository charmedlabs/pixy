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
with Interfaces.C.Strings;

package x86_64_linux_gnu_bits_sys_errlist_h is

   sys_nerr : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/sys_errlist.h:26
   pragma Import (C, sys_nerr, "sys_nerr");

   sys_errlist : aliased array (size_t) of Interfaces.C.Strings.chars_ptr;  -- /usr/include/x86_64-linux-gnu/bits/sys_errlist.h:27
   pragma Import (C, sys_errlist, "sys_errlist");

end x86_64_linux_gnu_bits_sys_errlist_h;
