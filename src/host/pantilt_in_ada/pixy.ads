pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with Interfaces; use Interfaces;

package pixy is

   type uint8  is mod 2**8;
   type uint16 is mod 2**16;
   type uint32 is mod 2**32;

   type int8 is range -(2**7) .. (2**7) - 1;
   for  int8'size use 8;

   type int16 is range -(2**15) .. (2**15) - 1;
   for  int16'size use 16;

   type int32 is range -(2**31) .. (2**31) - 1;
   for  int32'size use 32;

   type Sensor_Width  is range     0 ..  319;
   type Sensor_Height is range     0 ..  199;
   type RCS_Position  is range     0 .. 1000;
   type RCS_Error     is range -1000 .. 1000;

   RCS_Azimuth_Channel   : constant uint8 := 0;
   RCS_Altitude_Channel  : constant uint8 := 1;
   RCS_Pan_Channel       : constant uint8 := 0;
   RCS_Tilt_Channel      : constant uint8 := 1;
   Blocktype_Bormal      : constant int16 := 0;
   Blocktype_Color_Code  : constant int16 := 1;

   type Block is record
      c_type    : aliased uint16;  -- ../libpixyusb/include/pixy.h:73
      signature : aliased uint16;  -- ../libpixyusb/include/pixy.h:74
      x         : aliased uint16;  -- ../libpixyusb/include/pixy.h:75
      y         : aliased uint16;  -- ../libpixyusb/include/pixy.h:76
      width     : aliased uint16;  -- ../libpixyusb/include/pixy.h:77
      height    : aliased uint16;  -- ../libpixyusb/include/pixy.h:78
      angle     : aliased uint16;  -- ../libpixyusb/include/pixy.h:79
   end record;
   pragma Convention (C_Pass_By_Copy, Block);

   function init return int;  -- ../libpixyusb/include/pixy.h:90
   pragma Import (C, init, "pixy_init");

   function blocks_are_new return int;  -- ../libpixyusb/include/pixy.h:99
   pragma Import (C, blocks_are_new, "pixy_blocks_are_new");

   function get_blocks (max_blocks : uint16; blocks : access Block) return int;  -- ../libpixyusb/include/pixy.h:116
   pragma Import (C, get_blocks, "pixy_get_blocks");

   function command (name : Interfaces.C.Strings.chars_ptr  -- , ...
      ) return int;  -- ../libpixyusb/include/pixy.h:124
   pragma Import (C, command, "pixy_command");

   procedure close;  -- ../libpixyusb/include/pixy.h:129
   pragma Import (C, close, "pixy_close");

   procedure error (error_code : int);  -- ../libpixyusb/include/pixy.h:135
   pragma Import (C, error, "pixy_error");

   function led_set_RGB
     (red : uint8;
      green : uint8;
      blue : uint8) return int;  -- ../libpixyusb/include/pixy.h:145
   pragma Import (C, led_set_RGB, "pixy_led_set_RGB");

   function led_set_max_current (current : uint32) return int;  -- ../libpixyusb/include/pixy.h:153
   pragma Import (C, led_set_max_current, "pixy_led_set_max_current");

   function led_get_max_current return int;  -- ../libpixyusb/include/pixy.h:160
   pragma Import (C, led_get_max_current, "pixy_led_get_max_current");

   function cam_set_auto_white_balance (value : uint8) return int;  -- ../libpixyusb/include/pixy.h:169
   pragma Import (C, cam_set_auto_white_balance, "pixy_cam_set_auto_white_balance");

   function cam_get_auto_white_balance return int;  -- ../libpixyusb/include/pixy.h:177
   pragma Import (C, cam_get_auto_white_balance, "pixy_cam_get_auto_white_balance");

   function cam_get_white_balance_value return uint32;  -- ../libpixyusb/include/pixy.h:184
   pragma Import (C, cam_get_white_balance_value, "pixy_cam_get_white_balance_value");

   function cam_set_white_balance_value
     (red : uint8;
      green : uint8;
      blue : uint8) return int;  -- ../libpixyusb/include/pixy.h:194
   pragma Import (C, cam_set_white_balance_value, "pixy_cam_set_white_balance_value");

   function cam_set_auto_exposure_compensation (enable : uint8) return int;  -- ../libpixyusb/include/pixy.h:203
   pragma Import (C, cam_set_auto_exposure_compensation, "pixy_cam_set_auto_exposure_compensation");

   function cam_get_auto_exposure_compensation return int;  -- ../libpixyusb/include/pixy.h:211
   pragma Import (C, cam_get_auto_exposure_compensation, "pixy_cam_get_auto_exposure_compensation");

   function cam_set_exposure_compensation (gain : uint8; compensation : uint16) return int;  -- ../libpixyusb/include/pixy.h:220
   pragma Import (C, cam_set_exposure_compensation, "pixy_cam_set_exposure_compensation");

   function cam_get_exposure_compensation (gain : access uint8; compensation : access uint16) return int;  -- ../libpixyusb/include/pixy.h:229
   pragma Import (C, cam_get_exposure_compensation, "pixy_cam_get_exposure_compensation");

   function cam_set_brightness (brightness : uint8) return int;  -- ../libpixyusb/include/pixy.h:237
   pragma Import (C, cam_set_brightness, "pixy_cam_set_brightness");

   function cam_get_brightness return int;  -- ../libpixyusb/include/pixy.h:244
   pragma Import (C, cam_get_brightness, "pixy_cam_get_brightness");

   function rcs_get_position (channel : uint8) return int;  -- ../libpixyusb/include/pixy.h:252
   pragma Import (C, rcs_get_position, "pixy_rcs_get_position");

   function rcs_set_position (channel : uint8; position : uint16) return int;  -- ../libpixyusb/include/pixy.h:261
   pragma Import (C, rcs_set_position, "pixy_rcs_set_position");

   function rcs_set_frequency (frequency : uint16) return int;  -- ../libpixyusb/include/pixy.h:267
   pragma Import (C, rcs_set_frequency, "pixy_rcs_set_frequency");

   function get_firmware_version
     (major : access uint16;
      minor : access uint16;
      build : access uint16) return int;  -- ../libpixyusb/include/pixy.h:277
   pragma Import (C, get_firmware_version, "pixy_get_firmware_version");

end pixy;
