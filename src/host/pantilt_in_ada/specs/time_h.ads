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
with Interfaces.C.Strings;
with stddef_h;
with xlocale_h;
with unistd_h;
with System;

package time_h is

   --  unsupported macro: TIME_UTC 1
   subtype clock_t is x86_64_linux_gnu_bits_types_h.uu_clock_t;  -- /usr/include/time.h:59

   subtype time_t is x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/time.h:75

   subtype clockid_t is x86_64_linux_gnu_bits_types_h.uu_clockid_t;  -- /usr/include/time.h:91

   subtype timer_t is x86_64_linux_gnu_bits_types_h.uu_timer_t;  -- /usr/include/time.h:103

   type timespec is record
      tv_sec : aliased x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/time.h:122
      tv_nsec : aliased x86_64_linux_gnu_bits_types_h.uu_syscall_slong_t;  -- /usr/include/time.h:123
   end record;
   pragma Convention (C_Pass_By_Copy, timespec);  -- /usr/include/time.h:120

   type tm is record
      tm_sec : aliased int;  -- /usr/include/time.h:135
      tm_min : aliased int;  -- /usr/include/time.h:136
      tm_hour : aliased int;  -- /usr/include/time.h:137
      tm_mday : aliased int;  -- /usr/include/time.h:138
      tm_mon : aliased int;  -- /usr/include/time.h:139
      tm_year : aliased int;  -- /usr/include/time.h:140
      tm_wday : aliased int;  -- /usr/include/time.h:141
      tm_yday : aliased int;  -- /usr/include/time.h:142
      tm_isdst : aliased int;  -- /usr/include/time.h:143
      tm_gmtoff : aliased long;  -- /usr/include/time.h:146
      tm_zone : Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:147
   end record;
   pragma Convention (C_Pass_By_Copy, tm);  -- /usr/include/time.h:133

   type itimerspec is record
      it_interval : aliased timespec;  -- /usr/include/time.h:163
      it_value : aliased timespec;  -- /usr/include/time.h:164
   end record;
   pragma Convention (C_Pass_By_Copy, itimerspec);  -- /usr/include/time.h:161

   --  skipped empty struct sigevent

   function clock return clock_t;  -- /usr/include/time.h:189
   pragma Import (C, clock, "clock");

   function time (uu_timer : access time_t) return time_t;  -- /usr/include/time.h:192
   pragma Import (C, time, "time");

   function difftime (uu_time1 : time_t; uu_time0 : time_t) return double;  -- /usr/include/time.h:195
   pragma Import (C, difftime, "difftime");

   function mktime (uu_tp : access tm) return time_t;  -- /usr/include/time.h:199
   pragma Import (C, mktime, "mktime");

   function strftime
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_maxsize : stddef_h.size_t;
      uu_format : Interfaces.C.Strings.chars_ptr;
      uu_tp : access constant tm) return stddef_h.size_t;  -- /usr/include/time.h:205
   pragma Import (C, strftime, "strftime");

   function strptime
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_fmt : Interfaces.C.Strings.chars_ptr;
      uu_tp : access tm) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:213
   pragma Import (C, strptime, "strptime");

   function strftime_l
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_maxsize : stddef_h.size_t;
      uu_format : Interfaces.C.Strings.chars_ptr;
      uu_tp : access constant tm;
      uu_loc : xlocale_h.uu_locale_t) return stddef_h.size_t;  -- /usr/include/time.h:223
   pragma Import (C, strftime_l, "strftime_l");

   function strptime_l
     (uu_s : Interfaces.C.Strings.chars_ptr;
      uu_fmt : Interfaces.C.Strings.chars_ptr;
      uu_tp : access tm;
      uu_loc : xlocale_h.uu_locale_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:230
   pragma Import (C, strptime_l, "strptime_l");

   function gmtime (uu_timer : access time_t) return access tm;  -- /usr/include/time.h:239
   pragma Import (C, gmtime, "gmtime");

   function localtime (uu_timer : access time_t) return access tm;  -- /usr/include/time.h:243
   pragma Import (C, localtime, "localtime");

   function gmtime_r (uu_timer : access time_t; uu_tp : access tm) return access tm;  -- /usr/include/time.h:249
   pragma Import (C, gmtime_r, "gmtime_r");

   function localtime_r (uu_timer : access time_t; uu_tp : access tm) return access tm;  -- /usr/include/time.h:254
   pragma Import (C, localtime_r, "localtime_r");

   function asctime (uu_tp : access constant tm) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:261
   pragma Import (C, asctime, "asctime");

   function ctime (uu_timer : access time_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:264
   pragma Import (C, ctime, "ctime");

   function asctime_r (uu_tp : access constant tm; uu_buf : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:272
   pragma Import (C, asctime_r, "asctime_r");

   function ctime_r (uu_timer : access time_t; uu_buf : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:276
   pragma Import (C, ctime_r, "ctime_r");

   tzname : aliased array (0 .. 1) of Interfaces.C.Strings.chars_ptr;  -- /usr/include/time.h:289
   pragma Import (C, tzname, "tzname");

   procedure tzset;  -- /usr/include/time.h:293
   pragma Import (C, tzset, "tzset");

   daylight : aliased int;  -- /usr/include/time.h:297
   pragma Import (C, daylight, "daylight");

   timezone : aliased long;  -- /usr/include/time.h:298
   pragma Import (C, timezone, "timezone");

   function stime (uu_when : access time_t) return int;  -- /usr/include/time.h:304
   pragma Import (C, stime, "stime");

   function timegm (uu_tp : access tm) return time_t;  -- /usr/include/time.h:319
   pragma Import (C, timegm, "timegm");

   function timelocal (uu_tp : access tm) return time_t;  -- /usr/include/time.h:322
   pragma Import (C, timelocal, "timelocal");

   function dysize (uu_year : int) return int;  -- /usr/include/time.h:325
   pragma Import (C, dysize, "dysize");

   function nanosleep (uu_requested_time : access constant timespec; uu_remaining : access timespec) return int;  -- /usr/include/time.h:334
   pragma Import (C, nanosleep, "nanosleep");

   function clock_getres (uu_clock_id : clockid_t; uu_res : access timespec) return int;  -- /usr/include/time.h:339
   pragma Import (C, clock_getres, "clock_getres");

   function clock_gettime (uu_clock_id : clockid_t; uu_tp : access timespec) return int;  -- /usr/include/time.h:342
   pragma Import (C, clock_gettime, "clock_gettime");

   function clock_settime (uu_clock_id : clockid_t; uu_tp : access constant timespec) return int;  -- /usr/include/time.h:345
   pragma Import (C, clock_settime, "clock_settime");

   function clock_nanosleep
     (uu_clock_id : clockid_t;
      uu_flags : int;
      uu_req : access constant timespec;
      uu_rem : access timespec) return int;  -- /usr/include/time.h:353
   pragma Import (C, clock_nanosleep, "clock_nanosleep");

   function clock_getcpuclockid (uu_pid : unistd_h.pid_t; uu_clock_id : access clockid_t) return int;  -- /usr/include/time.h:358
   pragma Import (C, clock_getcpuclockid, "clock_getcpuclockid");

   function timer_create
     (uu_clock_id : clockid_t;
      uu_evp : System.Address;
      uu_timerid : System.Address) return int;  -- /usr/include/time.h:363
   pragma Import (C, timer_create, "timer_create");

   function timer_delete (uu_timerid : timer_t) return int;  -- /usr/include/time.h:368
   pragma Import (C, timer_delete, "timer_delete");

   function timer_settime
     (uu_timerid : timer_t;
      uu_flags : int;
      uu_value : access constant itimerspec;
      uu_ovalue : access itimerspec) return int;  -- /usr/include/time.h:371
   pragma Import (C, timer_settime, "timer_settime");

   function timer_gettime (uu_timerid : timer_t; uu_value : access itimerspec) return int;  -- /usr/include/time.h:376
   pragma Import (C, timer_gettime, "timer_gettime");

   function timer_getoverrun (uu_timerid : timer_t) return int;  -- /usr/include/time.h:380
   pragma Import (C, timer_getoverrun, "timer_getoverrun");

   function timespec_get (uu_ts : access timespec; uu_base : int) return int;  -- /usr/include/time.h:386
   pragma Import (C, timespec_get, "timespec_get");

   getdate_err : aliased int;  -- /usr/include/time.h:403
   pragma Import (C, getdate_err, "getdate_err");

   function getdate (uu_string : Interfaces.C.Strings.chars_ptr) return access tm;  -- /usr/include/time.h:412
   pragma Import (C, getdate, "getdate");

   function getdate_r (uu_string : Interfaces.C.Strings.chars_ptr; uu_resbufp : access tm) return int;  -- /usr/include/time.h:426
   pragma Import (C, getdate_r, "getdate_r");

end time_h;
