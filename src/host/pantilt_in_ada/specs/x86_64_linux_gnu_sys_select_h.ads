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
with x86_64_linux_gnu_bits_sigset_h;
limited with x86_64_linux_gnu_bits_time_h;
limited with time_h;

package x86_64_linux_gnu_sys_select_h is

   --  unsupported macro: FD_SETSIZE __FD_SETSIZE
   --  unsupported macro: NFDBITS __NFDBITS
   --  arg-macro: procedure FD_SET (fd, fdsetp)
   --    __FD_SET (fd, fdsetp)
   --  arg-macro: procedure FD_CLR (fd, fdsetp)
   --    __FD_CLR (fd, fdsetp)
   --  arg-macro: procedure FD_ISSET (fd, fdsetp)
   --    __FD_ISSET (fd, fdsetp)
   --  arg-macro: procedure FD_ZERO (fdsetp)
   --    __FD_ZERO (fdsetp)
   subtype sigset_t is x86_64_linux_gnu_bits_sigset_h.uu_sigset_t;

   subtype uu_fd_mask is long;  -- /usr/include/x86_64-linux-gnu/sys/select.h:54

   type fd_set_fds_bits_array is array (0 .. 15) of aliased uu_fd_mask;
   type fd_set is record
      fds_bits : aliased fd_set_fds_bits_array;  -- /usr/include/x86_64-linux-gnu/sys/select.h:69
   end record;
   pragma Convention (C_Pass_By_Copy, fd_set);  -- /usr/include/x86_64-linux-gnu/sys/select.h:75

   --  skipped anonymous struct anon_10

   subtype fd_mask is uu_fd_mask;  -- /usr/include/x86_64-linux-gnu/sys/select.h:82

   function c_select
     (uu_nfds : int;
      uu_readfds : access fd_set;
      uu_writefds : access fd_set;
      uu_exceptfds : access fd_set;
      uu_timeout : access x86_64_linux_gnu_bits_time_h.timeval) return int;  -- /usr/include/x86_64-linux-gnu/sys/select.h:106
   pragma Import (C, c_select, "select");

   function pselect
     (uu_nfds : int;
      uu_readfds : access fd_set;
      uu_writefds : access fd_set;
      uu_exceptfds : access fd_set;
      uu_timeout : access constant time_h.timespec;
      uu_sigmask : access constant x86_64_linux_gnu_bits_sigset_h.uu_sigset_t) return int;  -- /usr/include/x86_64-linux-gnu/sys/select.h:118
   pragma Import (C, pselect, "pselect");

end x86_64_linux_gnu_sys_select_h;
