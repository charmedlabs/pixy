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
with x86_64_linux_gnu_bits_types_h;
with stddef_h;

package libio_h is

   --  unsupported macro: EOF (-1)
   --  skipped empty struct u_IO_jump_t

   subtype u_IO_lock_t is System.Address;  -- /usr/include/libio.h:154

   type u_IO_FILE;
   type u_IO_marker is record
      u_next : access u_IO_marker;  -- /usr/include/libio.h:161
      u_sbuf : access u_IO_FILE;  -- /usr/include/libio.h:162
      u_pos : aliased int;  -- /usr/include/libio.h:166
   end record;
   pragma Convention (C_Pass_By_Copy, u_IO_marker);  -- /usr/include/libio.h:160

   type uu_codecvt_result is 
     (uu_codecvt_ok,
      uu_codecvt_partial,
      uu_codecvt_error,
      uu_codecvt_noconv);
   pragma Convention (C, uu_codecvt_result);  -- /usr/include/libio.h:180

   type u_IO_FILE is record
      u_flags : aliased int;  -- /usr/include/libio.h:246
      u_IO_read_ptr : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:251
      u_IO_read_end : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:252
      u_IO_read_base : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:253
      u_IO_write_base : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:254
      u_IO_write_ptr : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:255
      u_IO_write_end : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:256
      u_IO_buf_base : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:257
      u_IO_buf_end : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:258
      u_IO_save_base : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:260
      u_IO_backup_base : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:261
      u_IO_save_end : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:262
      u_markers : access u_IO_marker;  -- /usr/include/libio.h:264
      u_chain : access u_IO_FILE;  -- /usr/include/libio.h:266
      u_fileno : aliased int;  -- /usr/include/libio.h:268
      u_flags2 : aliased int;  -- /usr/include/libio.h:272
      u_old_offset : aliased x86_64_linux_gnu_bits_types_h.uu_off_t;  -- /usr/include/libio.h:274
      u_cur_column : aliased unsigned_short;  -- /usr/include/libio.h:278
      u_vtable_offset : aliased signed_char;  -- /usr/include/libio.h:279
      u_shortbuf : aliased u_IO_FILE_u_shortbuf_array;  -- /usr/include/libio.h:280
      u_lock : System.Address;  -- /usr/include/libio.h:284
      u_offset : aliased x86_64_linux_gnu_bits_types_h.uu_off64_t;  -- /usr/include/libio.h:293
      uu_pad1 : System.Address;  -- /usr/include/libio.h:302
      uu_pad2 : System.Address;  -- /usr/include/libio.h:303
      uu_pad3 : System.Address;  -- /usr/include/libio.h:304
      uu_pad4 : System.Address;  -- /usr/include/libio.h:305
      uu_pad5 : aliased stddef_h.size_t;  -- /usr/include/libio.h:306
      u_mode : aliased int;  -- /usr/include/libio.h:308
      u_unused2 : aliased u_IO_FILE_u_unused2_array;  -- /usr/include/libio.h:310
   end record;
   pragma Convention (C_Pass_By_Copy, u_IO_FILE);  -- /usr/include/libio.h:245

   --  skipped empty struct u_IO_FILE_plus

   --  skipped function type uu_io_read_fn

   --  skipped function type uu_io_write_fn

   --  skipped function type uu_io_seek_fn

   --  skipped function type uu_io_close_fn

   --  skipped function type cookie_read_function_t

   --  skipped function type cookie_write_function_t

   --  skipped function type cookie_seek_function_t

   --  skipped function type cookie_close_function_t

   type u_IO_cookie_io_functions_t is record
      read : access function
           (arg1 : System.Address;
            arg2 : Interfaces.C.Strings.chars_ptr;
            arg3 : stddef_h.size_t) return x86_64_linux_gnu_bits_types_h.uu_ssize_t;  -- /usr/include/libio.h:371
      write : access function
           (arg1 : System.Address;
            arg2 : Interfaces.C.Strings.chars_ptr;
            arg3 : stddef_h.size_t) return x86_64_linux_gnu_bits_types_h.uu_ssize_t;  -- /usr/include/libio.h:372
      seek : access function
           (arg1 : System.Address;
            arg2 : access x86_64_linux_gnu_bits_types_h.uu_off64_t;
            arg3 : int) return int;  -- /usr/include/libio.h:373
      close : access function (arg1 : System.Address) return int;  -- /usr/include/libio.h:374
   end record;
   pragma Convention (C_Pass_By_Copy, u_IO_cookie_io_functions_t);  -- /usr/include/libio.h:375

   --  skipped anonymous struct anon_5

   subtype cookie_io_functions_t is u_IO_cookie_io_functions_t;

   --  skipped empty struct u_IO_cookie_file

   --  skipped func _IO_cookie_init

   --  skipped func _IO_getc

   --  skipped func _IO_putc

   --  skipped func _IO_feof

   --  skipped func _IO_ferror

   --  skipped func _IO_peekc_locked

   --  skipped func _IO_flockfile

   --  skipped func _IO_funlockfile

   --  skipped func _IO_ftrylockfile

   --  skipped func _IO_vfscanf

   --  skipped func _IO_vfprintf

   --  skipped func _IO_padn

   --  skipped func _IO_sgetn

   --  skipped func _IO_seekoff

   --  skipped func _IO_seekpos

   --  skipped func _IO_free_backup_area

end libio_h;
