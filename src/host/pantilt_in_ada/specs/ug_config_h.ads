pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
with wchar_h;

package uG_config_h is

   type u_G_fpos_t is record
      uu_pos : aliased x86_64_linux_gnu_bits_types_h.uu_off_t;  -- /usr/include/_G_config.h:23
      uu_state : aliased wchar_h.uu_mbstate_t;  -- /usr/include/_G_config.h:24
   end record;
   pragma Convention (C_Pass_By_Copy, u_G_fpos_t);  -- /usr/include/_G_config.h:25

   --  skipped anonymous struct anon_3

   type u_G_fpos64_t is record
      uu_pos : aliased x86_64_linux_gnu_bits_types_h.uu_off64_t;  -- /usr/include/_G_config.h:28
      uu_state : aliased wchar_h.uu_mbstate_t;  -- /usr/include/_G_config.h:29
   end record;
   pragma Convention (C_Pass_By_Copy, u_G_fpos64_t);  -- /usr/include/_G_config.h:30

   --  skipped anonymous struct anon_4

end uG_config_h;
