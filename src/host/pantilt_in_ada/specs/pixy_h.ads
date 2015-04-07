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
with stdint_h;
with Interfaces.C.Strings;

package pixy_h is

   --  unsupported macro: PIXY_MAX_SIGNATURE 7
   --  unsupported macro: PIXY_MIN_X 0
   --  unsupported macro: PIXY_MAX_X 319
   --  unsupported macro: PIXY_MIN_Y 0
   --  unsupported macro: PIXY_MAX_Y 199
   --  unsupported macro: PIXY_RCS_MIN_POS 0
   --  unsupported macro: PIXY_RCS_MAX_POS 1000
   --  unsupported macro: PIXY_RCS_CENTER_POS ((PIXY_RCS_MAX_POS-PIXY_RCS_MIN_POS)/2)
   --  unsupported macro: PIXY_BLOCKTYPE_NORMAL 0
   --  unsupported macro: PIXY_BLOCKTYPE_COLOR_CODE 1
   package Class_Block is
      type Block is limited record
         c_type : aliased stdint_h.uint16_t;  -- ../../libpixyusb/include/pixy.h:73
         signature : aliased stdint_h.uint16_t;  -- ../../libpixyusb/include/pixy.h:74
         x : aliased stdint_h.uint16_t;  -- ../../libpixyusb/include/pixy.h:75
         y : aliased stdint_h.uint16_t;  -- ../../libpixyusb/include/pixy.h:76
         width : aliased stdint_h.uint16_t;  -- ../../libpixyusb/include/pixy.h:77
         height : aliased stdint_h.uint16_t;  -- ../../libpixyusb/include/pixy.h:78
         angle : aliased stdint_h.int16_t;  -- ../../libpixyusb/include/pixy.h:79
      end record;
      pragma Import (CPP, Block);

      procedure print (this : access Block; buf : Interfaces.C.Strings.chars_ptr);  -- ../../libpixyusb/include/pixy.h:50
      pragma Import (CPP, print, "_ZN5Block5printEPc");
   end;
   use Class_Block;
   function pixy_init return int;  -- ../../libpixyusb/include/pixy.h:90
   pragma Import (C, pixy_init, "pixy_init");

   function pixy_blocks_are_new return int;  -- ../../libpixyusb/include/pixy.h:99
   pragma Import (C, pixy_blocks_are_new, "pixy_blocks_are_new");

   function pixy_get_blocks (max_blocks : stdint_h.uint16_t; blocks : access Block) return int;  -- ../../libpixyusb/include/pixy.h:116
   pragma Import (C, pixy_get_blocks, "pixy_get_blocks");

   function pixy_command (name : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- ../../libpixyusb/include/pixy.h:124
   pragma Import (C, pixy_command, "pixy_command");

   procedure pixy_close;  -- ../../libpixyusb/include/pixy.h:129
   pragma Import (C, pixy_close, "pixy_close");

   procedure pixy_error (error_code : int);  -- ../../libpixyusb/include/pixy.h:135
   pragma Import (C, pixy_error, "pixy_error");

   function pixy_led_set_RGB
     (red : stdint_h.uint8_t;
      green : stdint_h.uint8_t;
      blue : stdint_h.uint8_t) return int;  -- ../../libpixyusb/include/pixy.h:145
   pragma Import (C, pixy_led_set_RGB, "pixy_led_set_RGB");

   function pixy_led_set_max_current (current : stdint_h.uint32_t) return int;  -- ../../libpixyusb/include/pixy.h:153
   pragma Import (C, pixy_led_set_max_current, "pixy_led_set_max_current");

   function pixy_led_get_max_current return int;  -- ../../libpixyusb/include/pixy.h:160
   pragma Import (C, pixy_led_get_max_current, "pixy_led_get_max_current");

   function pixy_cam_set_auto_white_balance (value : stdint_h.uint8_t) return int;  -- ../../libpixyusb/include/pixy.h:169
   pragma Import (C, pixy_cam_set_auto_white_balance, "pixy_cam_set_auto_white_balance");

   function pixy_cam_get_auto_white_balance return int;  -- ../../libpixyusb/include/pixy.h:177
   pragma Import (C, pixy_cam_get_auto_white_balance, "pixy_cam_get_auto_white_balance");

   function pixy_cam_get_white_balance_value return stdint_h.uint32_t;  -- ../../libpixyusb/include/pixy.h:184
   pragma Import (C, pixy_cam_get_white_balance_value, "pixy_cam_get_white_balance_value");

   function pixy_cam_set_white_balance_value
     (red : stdint_h.uint8_t;
      green : stdint_h.uint8_t;
      blue : stdint_h.uint8_t) return int;  -- ../../libpixyusb/include/pixy.h:194
   pragma Import (C, pixy_cam_set_white_balance_value, "pixy_cam_set_white_balance_value");

   function pixy_cam_set_auto_exposure_compensation (enable : stdint_h.uint8_t) return int;  -- ../../libpixyusb/include/pixy.h:203
   pragma Import (C, pixy_cam_set_auto_exposure_compensation, "pixy_cam_set_auto_exposure_compensation");

   function pixy_cam_get_auto_exposure_compensation return int;  -- ../../libpixyusb/include/pixy.h:211
   pragma Import (C, pixy_cam_get_auto_exposure_compensation, "pixy_cam_get_auto_exposure_compensation");

   function pixy_cam_set_exposure_compensation (gain : stdint_h.uint8_t; compensation : stdint_h.uint16_t) return int;  -- ../../libpixyusb/include/pixy.h:220
   pragma Import (C, pixy_cam_set_exposure_compensation, "pixy_cam_set_exposure_compensation");

   function pixy_cam_get_exposure_compensation (gain : access stdint_h.uint8_t; compensation : access stdint_h.uint16_t) return int;  -- ../../libpixyusb/include/pixy.h:229
   pragma Import (C, pixy_cam_get_exposure_compensation, "pixy_cam_get_exposure_compensation");

   function pixy_cam_set_brightness (brightness : stdint_h.uint8_t) return int;  -- ../../libpixyusb/include/pixy.h:237
   pragma Import (C, pixy_cam_set_brightness, "pixy_cam_set_brightness");

   function pixy_cam_get_brightness return int;  -- ../../libpixyusb/include/pixy.h:244
   pragma Import (C, pixy_cam_get_brightness, "pixy_cam_get_brightness");

   function pixy_rcs_get_position (channel : stdint_h.uint8_t) return int;  -- ../../libpixyusb/include/pixy.h:252
   pragma Import (C, pixy_rcs_get_position, "pixy_rcs_get_position");

   function pixy_rcs_set_position (channel : stdint_h.uint8_t; position : stdint_h.uint16_t) return int;  -- ../../libpixyusb/include/pixy.h:261
   pragma Import (C, pixy_rcs_set_position, "pixy_rcs_set_position");

   function pixy_rcs_set_frequency (frequency : stdint_h.uint16_t) return int;  -- ../../libpixyusb/include/pixy.h:267
   pragma Import (C, pixy_rcs_set_frequency, "pixy_rcs_set_frequency");

   function pixy_get_firmware_version
     (major : access stdint_h.uint16_t;
      minor : access stdint_h.uint16_t;
      build : access stdint_h.uint16_t) return int;  -- ../../libpixyusb/include/pixy.h:277
   pragma Import (C, pixy_get_firmware_version, "pixy_get_firmware_version");

end pixy_h;
