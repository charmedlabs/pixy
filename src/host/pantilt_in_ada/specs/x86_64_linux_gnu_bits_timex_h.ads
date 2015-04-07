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
with x86_64_linux_gnu_bits_time_h;

package x86_64_linux_gnu_bits_timex_h is

   --  unsupported macro: ADJ_OFFSET 0x0001
   --  unsupported macro: ADJ_FREQUENCY 0x0002
   --  unsupported macro: ADJ_MAXERROR 0x0004
   --  unsupported macro: ADJ_ESTERROR 0x0008
   --  unsupported macro: ADJ_STATUS 0x0010
   --  unsupported macro: ADJ_TIMECONST 0x0020
   --  unsupported macro: ADJ_TAI 0x0080
   --  unsupported macro: ADJ_MICRO 0x1000
   --  unsupported macro: ADJ_NANO 0x2000
   --  unsupported macro: ADJ_TICK 0x4000
   --  unsupported macro: ADJ_OFFSET_SINGLESHOT 0x8001
   --  unsupported macro: ADJ_OFFSET_SS_READ 0xa001
   --  unsupported macro: MOD_OFFSET ADJ_OFFSET
   --  unsupported macro: MOD_FREQUENCY ADJ_FREQUENCY
   --  unsupported macro: MOD_MAXERROR ADJ_MAXERROR
   --  unsupported macro: MOD_ESTERROR ADJ_ESTERROR
   --  unsupported macro: MOD_STATUS ADJ_STATUS
   --  unsupported macro: MOD_TIMECONST ADJ_TIMECONST
   --  unsupported macro: MOD_CLKB ADJ_TICK
   --  unsupported macro: MOD_CLKA ADJ_OFFSET_SINGLESHOT
   --  unsupported macro: MOD_TAI ADJ_TAI
   --  unsupported macro: MOD_MICRO ADJ_MICRO
   --  unsupported macro: MOD_NANO ADJ_NANO
   --  unsupported macro: STA_PLL 0x0001
   --  unsupported macro: STA_PPSFREQ 0x0002
   --  unsupported macro: STA_PPSTIME 0x0004
   --  unsupported macro: STA_FLL 0x0008
   --  unsupported macro: STA_INS 0x0010
   --  unsupported macro: STA_DEL 0x0020
   --  unsupported macro: STA_UNSYNC 0x0040
   --  unsupported macro: STA_FREQHOLD 0x0080
   --  unsupported macro: STA_PPSSIGNAL 0x0100
   --  unsupported macro: STA_PPSJITTER 0x0200
   --  unsupported macro: STA_PPSWANDER 0x0400
   --  unsupported macro: STA_PPSERROR 0x0800
   --  unsupported macro: STA_CLOCKERR 0x1000
   --  unsupported macro: STA_NANO 0x2000
   --  unsupported macro: STA_MODE 0x4000
   --  unsupported macro: STA_CLK 0x8000
   --  unsupported macro: STA_RONLY (STA_PPSSIGNAL | STA_PPSJITTER | STA_PPSWANDER | STA_PPSERROR | STA_CLOCKERR | STA_NANO | STA_MODE | STA_CLK)
   type timex is record
      modes : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:27
      offset : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:28
      freq : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:29
      maxerror : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:30
      esterror : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:31
      status : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:32
      c_constant : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:33
      precision : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:34
      tolerance : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:35
      time : aliased x86_64_linux_gnu_bits_time_h.timeval;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:36
      tick : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:37
      ppsfreq : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:38
      jitter : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:39
      shift : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:40
      stabil : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:41
      jitcnt : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:42
      calcnt : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:43
      errcnt : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:44
      stbcnt : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:45
      tai : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/timex.h:47
      field_21 : aliased int;
      field_22 : aliased int;
      field_23 : aliased int;
      field_24 : aliased int;
      field_25 : aliased int;
      field_26 : aliased int;
      field_27 : aliased int;
      field_28 : aliased int;
      field_29 : aliased int;
      field_30 : aliased int;
      field_31 : aliased int;
   end record;
   pragma Convention (C_Pass_By_Copy, timex);  -- /usr/include/x86_64-linux-gnu/bits/timex.h:25

end x86_64_linux_gnu_bits_timex_h;
