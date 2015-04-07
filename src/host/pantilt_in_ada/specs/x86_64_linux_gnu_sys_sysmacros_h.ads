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
with Interfaces.C.Extensions;

package x86_64_linux_gnu_sys_sysmacros_h is

   --  arg-macro: procedure major (dev)
   --    gnu_dev_major (dev)
   --  arg-macro: procedure minor (dev)
   --    gnu_dev_minor (dev)
   --  arg-macro: procedure makedev (maj, min)
   --    gnu_dev_makedev (maj, min)
   function gnu_dev_major (uu_dev : Extensions.unsigned_long_long) return unsigned;  -- /usr/include/x86_64-linux-gnu/sys/sysmacros.h:27
   pragma Import (C, gnu_dev_major, "gnu_dev_major");

   function gnu_dev_minor (uu_dev : Extensions.unsigned_long_long) return unsigned;  -- /usr/include/x86_64-linux-gnu/sys/sysmacros.h:30
   pragma Import (C, gnu_dev_minor, "gnu_dev_minor");

   function gnu_dev_makedev (uu_major : unsigned; uu_minor : unsigned) return Extensions.unsigned_long_long;  -- /usr/include/x86_64-linux-gnu/sys/sysmacros.h:33
   pragma Import (C, gnu_dev_makedev, "gnu_dev_makedev");

end x86_64_linux_gnu_sys_sysmacros_h;
