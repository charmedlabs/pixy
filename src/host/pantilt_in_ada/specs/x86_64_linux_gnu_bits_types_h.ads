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
with System;
with Interfaces.C.Strings;

package x86_64_linux_gnu_bits_types_h is

   subtype uu_u_char is unsigned_char;  -- /usr/include/x86_64-linux-gnu/bits/types.h:30

   subtype uu_u_short is unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/types.h:31

   subtype uu_u_int is unsigned;  -- /usr/include/x86_64-linux-gnu/bits/types.h:32

   subtype uu_u_long is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:33

   subtype uu_int8_t is signed_char;  -- /usr/include/x86_64-linux-gnu/bits/types.h:36

   subtype uu_uint8_t is unsigned_char;  -- /usr/include/x86_64-linux-gnu/bits/types.h:37

   subtype uu_int16_t is short;  -- /usr/include/x86_64-linux-gnu/bits/types.h:38

   subtype uu_uint16_t is unsigned_short;  -- /usr/include/x86_64-linux-gnu/bits/types.h:39

   subtype uu_int32_t is int;  -- /usr/include/x86_64-linux-gnu/bits/types.h:40

   subtype uu_uint32_t is unsigned;  -- /usr/include/x86_64-linux-gnu/bits/types.h:41

   subtype uu_int64_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:43

   subtype uu_uint64_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:44

   subtype uu_quad_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:52

   subtype uu_u_quad_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:53

   subtype uu_dev_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:124

   subtype uu_uid_t is unsigned;  -- /usr/include/x86_64-linux-gnu/bits/types.h:125

   subtype uu_gid_t is unsigned;  -- /usr/include/x86_64-linux-gnu/bits/types.h:126

   subtype uu_ino_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:127

   subtype uu_ino64_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:128

   subtype uu_mode_t is unsigned;  -- /usr/include/x86_64-linux-gnu/bits/types.h:129

   subtype uu_nlink_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:130

   subtype uu_off_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:131

   subtype uu_off64_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:132

   subtype uu_pid_t is int;  -- /usr/include/x86_64-linux-gnu/bits/types.h:133

   type uu_fsid_t_uu_val_array is array (0 .. 1) of aliased int;
   type uu_fsid_t is record
      uu_val : aliased uu_fsid_t_uu_val_array;  -- /usr/include/x86_64-linux-gnu/bits/types.h:134
   end record;
   pragma Convention (C_Pass_By_Copy, uu_fsid_t);  -- /usr/include/x86_64-linux-gnu/bits/types.h:134

   --  skipped anonymous struct anon_0

   subtype uu_clock_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:135

   subtype uu_rlim_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:136

   subtype uu_rlim64_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:137

   subtype uu_id_t is unsigned;  -- /usr/include/x86_64-linux-gnu/bits/types.h:138

   subtype uu_time_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:139

   subtype uu_useconds_t is unsigned;  -- /usr/include/x86_64-linux-gnu/bits/types.h:140

   subtype uu_suseconds_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:141

   subtype uu_daddr_t is int;  -- /usr/include/x86_64-linux-gnu/bits/types.h:143

   subtype uu_key_t is int;  -- /usr/include/x86_64-linux-gnu/bits/types.h:144

   subtype uu_clockid_t is int;  -- /usr/include/x86_64-linux-gnu/bits/types.h:147

   type uu_timer_t is new System.Address;  -- /usr/include/x86_64-linux-gnu/bits/types.h:150

   subtype uu_blksize_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:153

   subtype uu_blkcnt_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:158

   subtype uu_blkcnt64_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:159

   subtype uu_fsblkcnt_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:162

   subtype uu_fsblkcnt64_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:163

   subtype uu_fsfilcnt_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:166

   subtype uu_fsfilcnt64_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:167

   subtype uu_fsword_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:170

   subtype uu_ssize_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:172

   subtype uu_syscall_slong_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:175

   subtype uu_syscall_ulong_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:177

   subtype uu_loff_t is uu_off64_t;  -- /usr/include/x86_64-linux-gnu/bits/types.h:181

   type uu_qaddr_t is access all uu_quad_t;  -- /usr/include/x86_64-linux-gnu/bits/types.h:182

   type uu_caddr_t is new Interfaces.C.Strings.chars_ptr;  -- /usr/include/x86_64-linux-gnu/bits/types.h:183

   subtype uu_intptr_t is long;  -- /usr/include/x86_64-linux-gnu/bits/types.h:186

   subtype uu_socklen_t is unsigned;  -- /usr/include/x86_64-linux-gnu/bits/types.h:189

end x86_64_linux_gnu_bits_types_h;
