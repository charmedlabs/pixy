pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package x86_64_linux_gnu_sys_sysmacros_h is

   --  arg-macro: procedure major (dev)
   --    gnu_dev_major (dev)
   --  arg-macro: procedure minor (dev)
   --    gnu_dev_minor (dev)
   --  arg-macro: procedure makedev (maj, min)
   --    gnu_dev_makedev (maj, min)
   function gnu_dev_major (uu_dev : Extensions.unsigned_long_long) return unsigned;  -- /usr/include/x86_64-linux-gnu/sys/sysmacros.h:27
   pragma Import (C, gnu_dev_major, "gnu_dev_major");

   function gnu_dev_minor (uu_dev : Extensions.unsigned_long_long) return unsigned;  -- /usr/include/x86_64-linux-gnu/sys/sysmacros.h:30
   pragma Import (C, gnu_dev_minor, "gnu_dev_minor");

   function gnu_dev_makedev (uu_major : unsigned; uu_minor : unsigned) return Extensions.unsigned_long_long;  -- /usr/include/x86_64-linux-gnu/sys/sysmacros.h:33
   pragma Import (C, gnu_dev_makedev, "gnu_dev_makedev");

end x86_64_linux_gnu_sys_sysmacros_h;
