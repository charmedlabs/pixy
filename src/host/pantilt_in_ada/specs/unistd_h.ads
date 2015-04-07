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
with System;
with stddef_h;
with stdio_h;
with stdint_h;

package unistd_h is

   --  unsupported macro: STDIN_FILENO 0
   --  unsupported macro: STDOUT_FILENO 1
   --  unsupported macro: STDERR_FILENO 2
   --  unsupported macro: R_OK 4
   --  unsupported macro: W_OK 2
   --  unsupported macro: X_OK 1
   --  unsupported macro: F_OK 0
   --  unsupported macro: L_SET SEEK_SET
   --  unsupported macro: L_INCR SEEK_CUR
   --  unsupported macro: L_XTND SEEK_END
   --  unsupported macro: F_ULOCK 0
   --  unsupported macro: F_LOCK 1
   --  unsupported macro: F_TLOCK 2
   --  unsupported macro: F_TEST 3
   --  arg-macro: function TEMP_FAILURE_RETRY (expression)
   --    return __extension__ ({ long int __result; do __result := (long int) (expression); while (__result = -1L  and then  errno = EINTR); __result; });
   subtype gid_t is x86_64_linux_gnu_bits_types_h.uu_gid_t;  -- /usr/include/unistd.h:232

   subtype uid_t is x86_64_linux_gnu_bits_types_h.uu_uid_t;  -- /usr/include/unistd.h:237

   subtype useconds_t is x86_64_linux_gnu_bits_types_h.uu_useconds_t;  -- /usr/include/unistd.h:255

   subtype pid_t is x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/unistd.h:260

   subtype socklen_t is x86_64_linux_gnu_bits_types_h.uu_socklen_t;  -- /usr/include/unistd.h:274

   function c_access (uu_name : Interfaces.C.Strings.chars_ptr; uu_type : int) return int;  -- /usr/include/unistd.h:287
   pragma Import (C, c_access, "access");

   function euidaccess (uu_name : Interfaces.C.Strings.chars_ptr; uu_type : int) return int;  -- /usr/include/unistd.h:292
   pragma Import (C, euidaccess, "euidaccess");

   function eaccess (uu_name : Interfaces.C.Strings.chars_ptr; uu_type : int) return int;  -- /usr/include/unistd.h:296
   pragma Import (C, eaccess, "eaccess");

   function faccessat
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_type : int;
      uu_flag : int) return int;  -- /usr/include/unistd.h:304
   pragma Import (C, faccessat, "faccessat");

   function lseek
     (uu_fd : int;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off_t;
      uu_whence : int) return x86_64_linux_gnu_bits_types_h.uu_off_t;  -- /usr/include/unistd.h:334
   pragma Import (C, lseek, "lseek");

   function lseek64
     (uu_fd : int;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off64_t;
      uu_whence : int) return x86_64_linux_gnu_bits_types_h.uu_off64_t;  -- /usr/include/unistd.h:345
   pragma Import (C, lseek64, "lseek64");

   function close (uu_fd : int) return int;  -- /usr/include/unistd.h:353
   pragma Import (C, close, "close");

   function read
     (uu_fd : int;
      uu_buf : System.Address;
      uu_nbytes : stddef_h.size_t) return stdio_h.ssize_t;  -- /usr/include/unistd.h:360
   pragma Import (C, read, "read");

   function write
     (uu_fd : int;
      uu_buf : System.Address;
      uu_n : stddef_h.size_t) return stdio_h.ssize_t;  -- /usr/include/unistd.h:366
   pragma Import (C, write, "write");

   function pread
     (uu_fd : int;
      uu_buf : System.Address;
      uu_nbytes : stddef_h.size_t;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off_t) return stdio_h.ssize_t;  -- /usr/include/unistd.h:376
   pragma Import (C, pread, "pread");

   function pwrite
     (uu_fd : int;
      uu_buf : System.Address;
      uu_n : stddef_h.size_t;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off_t) return stdio_h.ssize_t;  -- /usr/include/unistd.h:384
   pragma Import (C, pwrite, "pwrite");

   function pread64
     (uu_fd : int;
      uu_buf : System.Address;
      uu_nbytes : stddef_h.size_t;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off64_t) return stdio_h.ssize_t;  -- /usr/include/unistd.h:404
   pragma Import (C, pread64, "pread64");

   function pwrite64
     (uu_fd : int;
      uu_buf : System.Address;
      uu_n : stddef_h.size_t;
      uu_offset : x86_64_linux_gnu_bits_types_h.uu_off64_t) return stdio_h.ssize_t;  -- /usr/include/unistd.h:408
   pragma Import (C, pwrite64, "pwrite64");

   function pipe (uu_pipedes : access int) return int;  -- /usr/include/unistd.h:417
   pragma Import (C, pipe, "pipe");

   function pipe2 (uu_pipedes : access int; uu_flags : int) return int;  -- /usr/include/unistd.h:422
   pragma Import (C, pipe2, "pipe2");

   function alarm (uu_seconds : unsigned) return unsigned;  -- /usr/include/unistd.h:432
   pragma Import (C, alarm, "alarm");

   function sleep (uu_seconds : unsigned) return unsigned;  -- /usr/include/unistd.h:444
   pragma Import (C, sleep, "sleep");

   function ualarm (uu_value : x86_64_linux_gnu_bits_types_h.uu_useconds_t; uu_interval : x86_64_linux_gnu_bits_types_h.uu_useconds_t) return x86_64_linux_gnu_bits_types_h.uu_useconds_t;  -- /usr/include/unistd.h:452
   pragma Import (C, ualarm, "ualarm");

   function usleep (uu_useconds : x86_64_linux_gnu_bits_types_h.uu_useconds_t) return int;  -- /usr/include/unistd.h:460
   pragma Import (C, usleep, "usleep");

   function pause return int;  -- /usr/include/unistd.h:469
   pragma Import (C, pause, "pause");

   function chown
     (uu_file : Interfaces.C.Strings.chars_ptr;
      uu_owner : x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_group : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int;  -- /usr/include/unistd.h:473
   pragma Import (C, chown, "chown");

   function fchown
     (uu_fd : int;
      uu_owner : x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_group : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int;  -- /usr/include/unistd.h:478
   pragma Import (C, fchown, "fchown");

   function lchown
     (uu_file : Interfaces.C.Strings.chars_ptr;
      uu_owner : x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_group : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int;  -- /usr/include/unistd.h:483
   pragma Import (C, lchown, "lchown");

   function fchownat
     (uu_fd : int;
      uu_file : Interfaces.C.Strings.chars_ptr;
      uu_owner : x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_group : x86_64_linux_gnu_bits_types_h.uu_gid_t;
      uu_flag : int) return int;  -- /usr/include/unistd.h:491
   pragma Import (C, fchownat, "fchownat");

   function chdir (uu_path : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/unistd.h:497
   pragma Import (C, chdir, "chdir");

   function fchdir (uu_fd : int) return int;  -- /usr/include/unistd.h:501
   pragma Import (C, fchdir, "fchdir");

   function getcwd (uu_buf : Interfaces.C.Strings.chars_ptr; uu_size : stddef_h.size_t) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/unistd.h:511
   pragma Import (C, getcwd, "getcwd");

   function get_current_dir_name return Interfaces.C.Strings.chars_ptr;  -- /usr/include/unistd.h:517
   pragma Import (C, get_current_dir_name, "get_current_dir_name");

   function getwd (uu_buf : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/unistd.h:525
   pragma Import (C, getwd, "getwd");

   function dup (uu_fd : int) return int;  -- /usr/include/unistd.h:531
   pragma Import (C, dup, "dup");

   function dup2 (uu_fd : int; uu_fd2 : int) return int;  -- /usr/include/unistd.h:534
   pragma Import (C, dup2, "dup2");

   function dup3
     (uu_fd : int;
      uu_fd2 : int;
      uu_flags : int) return int;  -- /usr/include/unistd.h:539
   pragma Import (C, dup3, "dup3");

   environ : System.Address;  -- /usr/include/unistd.h:545
   pragma Import (C, environ, "environ");

   function execve
     (uu_path : Interfaces.C.Strings.chars_ptr;
      uu_argv : System.Address;
      uu_envp : System.Address) return int;  -- /usr/include/unistd.h:551
   pragma Import (C, execve, "execve");

   function fexecve
     (uu_fd : int;
      uu_argv : System.Address;
      uu_envp : System.Address) return int;  -- /usr/include/unistd.h:557
   pragma Import (C, fexecve, "fexecve");

   function execv (uu_path : Interfaces.C.Strings.chars_ptr; uu_argv : System.Address) return int;  -- /usr/include/unistd.h:563
   pragma Import (C, execv, "execv");

   function execle (uu_path : Interfaces.C.Strings.chars_ptr; uu_arg : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/unistd.h:568
   pragma Import (C, execle, "execle");

   function execl (uu_path : Interfaces.C.Strings.chars_ptr; uu_arg : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/unistd.h:573
   pragma Import (C, execl, "execl");

   function execvp (uu_file : Interfaces.C.Strings.chars_ptr; uu_argv : System.Address) return int;  -- /usr/include/unistd.h:578
   pragma Import (C, execvp, "execvp");

   function execlp (uu_file : Interfaces.C.Strings.chars_ptr; uu_arg : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- /usr/include/unistd.h:584
   pragma Import (C, execlp, "execlp");

   function execvpe
     (uu_file : Interfaces.C.Strings.chars_ptr;
      uu_argv : System.Address;
      uu_envp : System.Address) return int;  -- /usr/include/unistd.h:590
   pragma Import (C, execvpe, "execvpe");

   function nice (uu_inc : int) return int;  -- /usr/include/unistd.h:598
   pragma Import (C, nice, "nice");

   --  skipped func _exit

   function pathconf (uu_path : Interfaces.C.Strings.chars_ptr; uu_name : int) return long;  -- /usr/include/unistd.h:612
   pragma Import (C, pathconf, "pathconf");

   function fpathconf (uu_fd : int; uu_name : int) return long;  -- /usr/include/unistd.h:616
   pragma Import (C, fpathconf, "fpathconf");

   function sysconf (uu_name : int) return long;  -- /usr/include/unistd.h:619
   pragma Import (C, sysconf, "sysconf");

   function confstr
     (uu_name : int;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_len : stddef_h.size_t) return stddef_h.size_t;  -- /usr/include/unistd.h:623
   pragma Import (C, confstr, "confstr");

   function getpid return x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/unistd.h:628
   pragma Import (C, getpid, "getpid");

   function getppid return x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/unistd.h:631
   pragma Import (C, getppid, "getppid");

   function getpgrp return x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/unistd.h:634
   pragma Import (C, getpgrp, "getpgrp");

   function getpgid (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t) return x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/unistd.h:639
   pragma Import (C, getpgid, "getpgid");

   function setpgid (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t; uu_pgid : x86_64_linux_gnu_bits_types_h.uu_pid_t) return int;  -- /usr/include/unistd.h:646
   pragma Import (C, setpgid, "setpgid");

   function setpgrp return int;  -- /usr/include/unistd.h:660
   pragma Import (C, setpgrp, "setpgrp");

   function setsid return x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/unistd.h:667
   pragma Import (C, setsid, "setsid");

   function getsid (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t) return x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/unistd.h:671
   pragma Import (C, getsid, "getsid");

   function getuid return x86_64_linux_gnu_bits_types_h.uu_uid_t;  -- /usr/include/unistd.h:675
   pragma Import (C, getuid, "getuid");

   function geteuid return x86_64_linux_gnu_bits_types_h.uu_uid_t;  -- /usr/include/unistd.h:678
   pragma Import (C, geteuid, "geteuid");

   function getgid return x86_64_linux_gnu_bits_types_h.uu_gid_t;  -- /usr/include/unistd.h:681
   pragma Import (C, getgid, "getgid");

   function getegid return x86_64_linux_gnu_bits_types_h.uu_gid_t;  -- /usr/include/unistd.h:684
   pragma Import (C, getegid, "getegid");

   function getgroups (uu_size : int; uu_list : access x86_64_linux_gnu_bits_types_h.uu_gid_t) return int;  -- /usr/include/unistd.h:689
   pragma Import (C, getgroups, "getgroups");

   function group_member (uu_gid : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int;  -- /usr/include/unistd.h:693
   pragma Import (C, group_member, "group_member");

   function setuid (uu_uid : x86_64_linux_gnu_bits_types_h.uu_uid_t) return int;  -- /usr/include/unistd.h:700
   pragma Import (C, setuid, "setuid");

   function setreuid (uu_ruid : x86_64_linux_gnu_bits_types_h.uu_uid_t; uu_euid : x86_64_linux_gnu_bits_types_h.uu_uid_t) return int;  -- /usr/include/unistd.h:705
   pragma Import (C, setreuid, "setreuid");

   function seteuid (uu_uid : x86_64_linux_gnu_bits_types_h.uu_uid_t) return int;  -- /usr/include/unistd.h:710
   pragma Import (C, seteuid, "seteuid");

   function setgid (uu_gid : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int;  -- /usr/include/unistd.h:717
   pragma Import (C, setgid, "setgid");

   function setregid (uu_rgid : x86_64_linux_gnu_bits_types_h.uu_gid_t; uu_egid : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int;  -- /usr/include/unistd.h:722
   pragma Import (C, setregid, "setregid");

   function setegid (uu_gid : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int;  -- /usr/include/unistd.h:727
   pragma Import (C, setegid, "setegid");

   function getresuid
     (uu_ruid : access x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_euid : access x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_suid : access x86_64_linux_gnu_bits_types_h.uu_uid_t) return int;  -- /usr/include/unistd.h:733
   pragma Import (C, getresuid, "getresuid");

   function getresgid
     (uu_rgid : access x86_64_linux_gnu_bits_types_h.uu_gid_t;
      uu_egid : access x86_64_linux_gnu_bits_types_h.uu_gid_t;
      uu_sgid : access x86_64_linux_gnu_bits_types_h.uu_gid_t) return int;  -- /usr/include/unistd.h:738
   pragma Import (C, getresgid, "getresgid");

   function setresuid
     (uu_ruid : x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_euid : x86_64_linux_gnu_bits_types_h.uu_uid_t;
      uu_suid : x86_64_linux_gnu_bits_types_h.uu_uid_t) return int;  -- /usr/include/unistd.h:743
   pragma Import (C, setresuid, "setresuid");

   function setresgid
     (uu_rgid : x86_64_linux_gnu_bits_types_h.uu_gid_t;
      uu_egid : x86_64_linux_gnu_bits_types_h.uu_gid_t;
      uu_sgid : x86_64_linux_gnu_bits_types_h.uu_gid_t) return int;  -- /usr/include/unistd.h:748
   pragma Import (C, setresgid, "setresgid");

   function fork return x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/unistd.h:756
   pragma Import (C, fork, "fork");

   function vfork return x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/unistd.h:764
   pragma Import (C, vfork, "vfork");

   function ttyname (uu_fd : int) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/unistd.h:770
   pragma Import (C, ttyname, "ttyname");

   function ttyname_r
     (uu_fd : int;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_buflen : stddef_h.size_t) return int;  -- /usr/include/unistd.h:774
   pragma Import (C, ttyname_r, "ttyname_r");

   function isatty (uu_fd : int) return int;  -- /usr/include/unistd.h:779
   pragma Import (C, isatty, "isatty");

   function ttyslot return int;  -- /usr/include/unistd.h:785
   pragma Import (C, ttyslot, "ttyslot");

   function link (uu_from : Interfaces.C.Strings.chars_ptr; uu_to : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/unistd.h:790
   pragma Import (C, link, "link");

   function linkat
     (uu_fromfd : int;
      uu_from : Interfaces.C.Strings.chars_ptr;
      uu_tofd : int;
      uu_to : Interfaces.C.Strings.chars_ptr;
      uu_flags : int) return int;  -- /usr/include/unistd.h:796
   pragma Import (C, linkat, "linkat");

   function symlink (uu_from : Interfaces.C.Strings.chars_ptr; uu_to : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/unistd.h:803
   pragma Import (C, symlink, "symlink");

   function readlink
     (uu_path : Interfaces.C.Strings.chars_ptr;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_len : stddef_h.size_t) return stdio_h.ssize_t;  -- /usr/include/unistd.h:809
   pragma Import (C, readlink, "readlink");

   function symlinkat
     (uu_from : Interfaces.C.Strings.chars_ptr;
      uu_tofd : int;
      uu_to : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/unistd.h:816
   pragma Import (C, symlinkat, "symlinkat");

   function readlinkat
     (uu_fd : int;
      uu_path : Interfaces.C.Strings.chars_ptr;
      uu_buf : Interfaces.C.Strings.chars_ptr;
      uu_len : stddef_h.size_t) return stdio_h.ssize_t;  -- /usr/include/unistd.h:820
   pragma Import (C, readlinkat, "readlinkat");

   function unlink (uu_name : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/unistd.h:826
   pragma Import (C, unlink, "unlink");

   function unlinkat
     (uu_fd : int;
      uu_name : Interfaces.C.Strings.chars_ptr;
      uu_flag : int) return int;  -- /usr/include/unistd.h:830
   pragma Import (C, unlinkat, "unlinkat");

   function rmdir (uu_path : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/unistd.h:835
   pragma Import (C, rmdir, "rmdir");

   function tcgetpgrp (uu_fd : int) return x86_64_linux_gnu_bits_types_h.uu_pid_t;  -- /usr/include/unistd.h:839
   pragma Import (C, tcgetpgrp, "tcgetpgrp");

   function tcsetpgrp (uu_fd : int; uu_pgrp_id : x86_64_linux_gnu_bits_types_h.uu_pid_t) return int;  -- /usr/include/unistd.h:842
   pragma Import (C, tcsetpgrp, "tcsetpgrp");

   function getlogin return Interfaces.C.Strings.chars_ptr;  -- /usr/include/unistd.h:849
   pragma Import (C, getlogin, "getlogin");

   function getlogin_r (uu_name : Interfaces.C.Strings.chars_ptr; uu_name_len : stddef_h.size_t) return int;  -- /usr/include/unistd.h:857
   pragma Import (C, getlogin_r, "getlogin_r");

   function setlogin (uu_name : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/unistd.h:862
   pragma Import (C, setlogin, "setlogin");

   function gethostname (uu_name : Interfaces.C.Strings.chars_ptr; uu_len : stddef_h.size_t) return int;  -- /usr/include/unistd.h:879
   pragma Import (C, gethostname, "gethostname");

   function sethostname (uu_name : Interfaces.C.Strings.chars_ptr; uu_len : stddef_h.size_t) return int;  -- /usr/include/unistd.h:886
   pragma Import (C, sethostname, "sethostname");

   function sethostid (uu_id : long) return int;  -- /usr/include/unistd.h:891
   pragma Import (C, sethostid, "sethostid");

   function getdomainname (uu_name : Interfaces.C.Strings.chars_ptr; uu_len : stddef_h.size_t) return int;  -- /usr/include/unistd.h:897
   pragma Import (C, getdomainname, "getdomainname");

   function setdomainname (uu_name : Interfaces.C.Strings.chars_ptr; uu_len : stddef_h.size_t) return int;  -- /usr/include/unistd.h:899
   pragma Import (C, setdomainname, "setdomainname");

   function vhangup return int;  -- /usr/include/unistd.h:906
   pragma Import (C, vhangup, "vhangup");

   function revoke (uu_file : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/unistd.h:909
   pragma Import (C, revoke, "revoke");

   function profil
     (uu_sample_buffer : access unsigned_short;
      uu_size : stddef_h.size_t;
      uu_offset : stddef_h.size_t;
      uu_scale : unsigned) return int;  -- /usr/include/unistd.h:917
   pragma Import (C, profil, "profil");

   function acct (uu_name : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/unistd.h:925
   pragma Import (C, acct, "acct");

   function getusershell return Interfaces.C.Strings.chars_ptr;  -- /usr/include/unistd.h:929
   pragma Import (C, getusershell, "getusershell");

   procedure endusershell;  -- /usr/include/unistd.h:930
   pragma Import (C, endusershell, "endusershell");

   procedure setusershell;  -- /usr/include/unistd.h:931
   pragma Import (C, setusershell, "setusershell");

   function daemon (uu_nochdir : int; uu_noclose : int) return int;  -- /usr/include/unistd.h:937
   pragma Import (C, daemon, "daemon");

   function chroot (uu_path : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/unistd.h:944
   pragma Import (C, chroot, "chroot");

   function getpass (uu_prompt : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/unistd.h:948
   pragma Import (C, getpass, "getpass");

   function fsync (uu_fd : int) return int;  -- /usr/include/unistd.h:956
   pragma Import (C, fsync, "fsync");

   function syncfs (uu_fd : int) return int;  -- /usr/include/unistd.h:962
   pragma Import (C, syncfs, "syncfs");

   function gethostid return long;  -- /usr/include/unistd.h:969
   pragma Import (C, gethostid, "gethostid");

   procedure sync;  -- /usr/include/unistd.h:972
   pragma Import (C, sync, "sync");

   function getpagesize return int;  -- /usr/include/unistd.h:978
   pragma Import (C, getpagesize, "getpagesize");

   function getdtablesize return int;  -- /usr/include/unistd.h:983
   pragma Import (C, getdtablesize, "getdtablesize");

   function truncate (uu_file : Interfaces.C.Strings.chars_ptr; uu_length : x86_64_linux_gnu_bits_types_h.uu_off_t) return int;  -- /usr/include/unistd.h:993
   pragma Import (C, truncate, "truncate");

   function truncate64 (uu_file : Interfaces.C.Strings.chars_ptr; uu_length : x86_64_linux_gnu_bits_types_h.uu_off64_t) return int;  -- /usr/include/unistd.h:1005
   pragma Import (C, truncate64, "truncate64");

   function ftruncate (uu_fd : int; uu_length : x86_64_linux_gnu_bits_types_h.uu_off_t) return int;  -- /usr/include/unistd.h:1016
   pragma Import (C, ftruncate, "ftruncate");

   function ftruncate64 (uu_fd : int; uu_length : x86_64_linux_gnu_bits_types_h.uu_off64_t) return int;  -- /usr/include/unistd.h:1026
   pragma Import (C, ftruncate64, "ftruncate64");

   function brk (uu_addr : System.Address) return int;  -- /usr/include/unistd.h:1037
   pragma Import (C, brk, "brk");

   function sbrk (uu_delta : stdint_h.intptr_t) return System.Address;  -- /usr/include/unistd.h:1043
   pragma Import (C, sbrk, "sbrk");

   function syscall (uu_sysno : long  -- , ...
      ) return long;  -- /usr/include/unistd.h:1058
   pragma Import (C, syscall, "syscall");

   function lockf
     (uu_fd : int;
      uu_cmd : int;
      uu_len : x86_64_linux_gnu_bits_types_h.uu_off_t) return int;  -- /usr/include/unistd.h:1081
   pragma Import (C, lockf, "lockf");

   function lockf64
     (uu_fd : int;
      uu_cmd : int;
      uu_len : x86_64_linux_gnu_bits_types_h.uu_off64_t) return int;  -- /usr/include/unistd.h:1091
   pragma Import (C, lockf64, "lockf64");

   function fdatasync (uu_fildes : int) return int;  -- /usr/include/unistd.h:1112
   pragma Import (C, fdatasync, "fdatasync");

   function crypt (uu_key : Interfaces.C.Strings.chars_ptr; uu_salt : Interfaces.C.Strings.chars_ptr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/unistd.h:1120
   pragma Import (C, crypt, "crypt");

   procedure encrypt (uu_glibc_block : Interfaces.C.Strings.chars_ptr; uu_edflag : int);  -- /usr/include/unistd.h:1125
   pragma Import (C, encrypt, "encrypt");

   procedure swab
     (uu_from : System.Address;
      uu_to : System.Address;
      uu_n : stdio_h.ssize_t);  -- /usr/include/unistd.h:1133
   pragma Import (C, swab, "swab");

end unistd_h;
