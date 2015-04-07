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
