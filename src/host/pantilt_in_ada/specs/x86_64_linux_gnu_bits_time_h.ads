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
with x86_64_linux_gnu_bits_types_h;
limited with x86_64_linux_gnu_bits_timex_h;

package x86_64_linux_gnu_bits_time_h is

   --  unsupported macro: CLOCKS_PER_SEC 1000000l
   --  unsupported macro: CLOCK_REALTIME 0
   --  unsupported macro: CLOCK_MONOTONIC 1
   --  unsupported macro: CLOCK_PROCESS_CPUTIME_ID 2
   --  unsupported macro: CLOCK_THREAD_CPUTIME_ID 3
   --  unsupported macro: CLOCK_MONOTONIC_RAW 4
   --  unsupported macro: CLOCK_REALTIME_COARSE 5
   --  unsupported macro: CLOCK_MONOTONIC_COARSE 6
   --  unsupported macro: CLOCK_BOOTTIME 7
   --  unsupported macro: CLOCK_REALTIME_ALARM 8
   --  unsupported macro: CLOCK_BOOTTIME_ALARM 9
   --  unsupported macro: TIMER_ABSTIME 1
   type timeval is record
      tv_sec : aliased x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/x86_64-linux-gnu/bits/time.h:32
      tv_usec : aliased x86_64_linux_gnu_bits_types_h.uu_suseconds_t;  -- /usr/include/x86_64-linux-gnu/bits/time.h:33
   end record;
   pragma Convention (C_Pass_By_Copy, timeval);  -- /usr/include/x86_64-linux-gnu/bits/time.h:30

   function clock_adjtime (uu_clock_id : x86_64_linux_gnu_bits_types_h.uu_clockid_t; uu_utx : access x86_64_linux_gnu_bits_timex_h.timex) return int;  -- /usr/include/x86_64-linux-gnu/bits/time.h:91
   pragma Import (C, clock_adjtime, "clock_adjtime");

end x86_64_linux_gnu_bits_time_h;
