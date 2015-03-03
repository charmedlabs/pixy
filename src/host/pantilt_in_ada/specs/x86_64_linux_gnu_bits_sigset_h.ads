pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package x86_64_linux_gnu_bits_sigset_h is

   subtype uu_sig_atomic_t is int;  -- /usr/include/x86_64-linux-gnu/bits/sigset.h:22

   type uu_sigset_t_uu_val_array is array (0 .. 15) of aliased unsigned_long;
   type uu_sigset_t is record
      uu_val : aliased uu_sigset_t_uu_val_array;  -- /usr/include/x86_64-linux-gnu/bits/sigset.h:29
   end record;
   pragma Convention (C_Pass_By_Copy, uu_sigset_t);  -- /usr/include/x86_64-linux-gnu/bits/sigset.h:30

   --  skipped anonymous struct anon_9

end x86_64_linux_gnu_bits_sigset_h;
