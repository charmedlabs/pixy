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
with x86_64_linux_gnu_bits_time_h;
with Interfaces.C.Strings;

package x86_64_linux_gnu_sys_time_h is

   --  arg-macro: procedure TIMEVAL_TO_TIMESPEC (tv, ts)
   --    { (ts).tv_sec := (tv).tv_sec; (ts).tv_nsec := (tv).tv_usec * 1000; }
   --  arg-macro: procedure TIMESPEC_TO_TIMEVAL (tv, ts)
   --    { (tv).tv_sec := (ts).tv_sec; (tv).tv_usec := (ts).tv_nsec / 1000; }
   --  unsupported macro: ITIMER_REAL ITIMER_REAL
   --  unsupported macro: ITIMER_VIRTUAL ITIMER_VIRTUAL
   --  unsupported macro: ITIMER_PROF ITIMER_PROF
   --  arg-macro: function timerisset (tvp)
   --    return (tvp).tv_sec  or else  (tvp).tv_usec;
   --  arg-macro: function timerclear (tvp)
   --    return (tvp).tv_sec := (tvp).tv_usec := 0;
   --  arg-macro: function timercmp (a, b, CMP)
   --    return ((a).tv_sec = (b).tv_sec) ? ((a).tv_usec CMP (b).tv_usec) : ((a).tv_sec CMP (b).tv_sec);
   --  arg-macro: procedure timeradd (a, b, result)
   --    do { (result).tv_sec := (a).tv_sec + (b).tv_sec; (result).tv_usec := (a).tv_usec + (b).tv_usec; if ((result).tv_usec >= 1000000) { ++(result).tv_sec; (result).tv_usec -= 1000000; } } while (0)
   --  arg-macro: procedure timersub (a, b, result)
   --    do { (result).tv_sec := (a).tv_sec - (b).tv_sec; (result).tv_usec := (a).tv_usec - (b).tv_usec; if ((result).tv_usec < 0) { --(result).tv_sec; (result).tv_usec += 1000000; } } while (0)
   type timezone is record
      tz_minuteswest : aliased int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:57
      tz_dsttime : aliased int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:58
   end record;
   pragma Convention (C_Pass_By_Copy, timezone);  -- /usr/include/x86_64-linux-gnu/sys/time.h:55

   type uu_timezone_ptr_t is access all timezone;  -- /usr/include/x86_64-linux-gnu/sys/time.h:61

   function gettimeofday (uu_tv : access x86_64_linux_gnu_bits_time_h.timeval; uu_tz : uu_timezone_ptr_t) return int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:71
   pragma Import (C, gettimeofday, "gettimeofday");

   function settimeofday (uu_tv : access constant x86_64_linux_gnu_bits_time_h.timeval; uu_tz : access constant timezone) return int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:77
   pragma Import (C, settimeofday, "settimeofday");

   function adjtime (uu_delta : access constant x86_64_linux_gnu_bits_time_h.timeval; uu_olddelta : access x86_64_linux_gnu_bits_time_h.timeval) return int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:85
   pragma Import (C, adjtime, "adjtime");

   type uu_itimer_which is 
     (ITIMER_REAL,
      ITIMER_VIRTUAL,
      ITIMER_PROF);
   pragma Convention (C, uu_itimer_which);  -- /usr/include/x86_64-linux-gnu/sys/time.h:91

   type itimerval is record
      it_interval : aliased x86_64_linux_gnu_bits_time_h.timeval;  -- /usr/include/x86_64-linux-gnu/sys/time.h:110
      it_value : aliased x86_64_linux_gnu_bits_time_h.timeval;  -- /usr/include/x86_64-linux-gnu/sys/time.h:112
   end record;
   pragma Convention (C_Pass_By_Copy, itimerval);  -- /usr/include/x86_64-linux-gnu/sys/time.h:107

   subtype uu_itimer_which_t is int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:120

   function getitimer (uu_which : uu_itimer_which_t; uu_value : access itimerval) return int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:125
   pragma Import (C, getitimer, "getitimer");

   function setitimer
     (uu_which : uu_itimer_which_t;
      uu_new : access constant itimerval;
      uu_old : access itimerval) return int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:131
   pragma Import (C, setitimer, "setitimer");

   function utimes (uu_file : Interfaces.C.Strings.chars_ptr; uu_tvp : access constant x86_64_linux_gnu_bits_time_h.timeval) return int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:138
   pragma Import (C, utimes, "utimes");

   function lutimes (uu_file : Interfaces.C.Strings.chars_ptr; uu_tvp : access constant x86_64_linux_gnu_bits_time_h.timeval) return int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:143
   pragma Import (C, lutimes, "lutimes");

   function futimes (uu_fd : int; uu_tvp : access constant x86_64_linux_gnu_bits_time_h.timeval) return int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:147
   pragma Import (C, futimes, "futimes");

   function futimesat
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_tvp : access constant x86_64_linux_gnu_bits_time_h.timeval) return int;  -- /usr/include/x86_64-linux-gnu/sys/time.h:154
   pragma Import (C, futimesat, "futimesat");

end x86_64_linux_gnu_sys_time_h;
