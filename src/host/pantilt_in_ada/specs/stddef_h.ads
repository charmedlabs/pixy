pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package stddef_h is

   --  unsupported macro: NULL __null
   subtype size_t is unsigned_long;  -- /usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h:212

end stddef_h;
