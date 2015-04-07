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
with System;

package getopt_h is

   optarg : Interfaces.C.Strings.chars_ptr;  -- /usr/include/getopt.h:57
   pragma Import (C, optarg, "optarg");

   optind : aliased int;  -- /usr/include/getopt.h:71
   pragma Import (C, optind, "optind");

   opterr : aliased int;  -- /usr/include/getopt.h:76
   pragma Import (C, opterr, "opterr");

   optopt : aliased int;  -- /usr/include/getopt.h:80
   pragma Import (C, optopt, "optopt");

   function getopt
     (uuu_argc : int;
      uuu_argv : System.Address;
      uu_shortopts : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/getopt.h:150
   pragma Import (C, getopt, "getopt");

end getopt_h;
