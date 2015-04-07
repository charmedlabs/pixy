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
--  Pixy Tracking Demo - Ada Version --
--
--  To Build:
--
--  gnatmake pan_tilt.adb -aO/usr/local/lib -aO/usr/lib/gcc/x86_64-linux-gnu/4.8/ -aO/usr/lib/x86_64-linux-gnu/ -largs -lpthread -lpixyusb -lboost_system -lboost_chrono -lboost_thread -lstdc++ -lusb-1.0

with Ada.Text_IO;    use Ada.Text_IO;
with Interfaces;     use Interfaces;
with Interfaces.C;   use Interfaces.C;
with Pixy;           use Pixy;
with SIGINT_Handler; use SIGINT_Handler;

function Pan_Tilt return int is

  Pixy_X_Center   : constant Sensor_Width  := (Sensor_Width'Last - Sensor_Width'First) / 2;
  Pixy_Y_Center   : constant Sensor_Height := (Sensor_Height'Last - Sensor_Height'First) / 2;
  Pixy_RCS_Center : constant RCS_Position  := (RCS_Position'Last - RCS_Position'First) / 2;

  -- PID Control Parameters --
  Azimuth_Proportional_Gain  : constant integer := 400;
  Azimuth_Derivative_Gain    : constant integer := 300;
  Altitude_Proportional_Gain : constant integer := 500;
  Altitude_Derivative_Gain   : constant integer := 400;

  type Gimbal is record
    Position             : RCS_Position;
    Error                : RCS_Error;
    Previous_Error       : RCS_Error;
    Previous_Error_Valid : boolean;
  end record;

  Block : aliased Pixy.Block;

  type Azimuth_Type is new Gimbal;
  type Altitude_Type is new Gimbal;

  Azimuth  : Azimuth_Type;
  Altitude : Altitude_Type;

  Pixy_Init_Status : int;
  Blocks_Copied    : int;
  Result           : int;
  Frame_Index      : integer;

  procedure Initialize_Gimbals is
  begin
    Azimuth.Position              := Pixy_RCS_Center;
    Azimuth.Previous_Error_Valid  := false;
    Altitude.Position             := Pixy_RCS_Center;
    Altitude.Previous_Error_Valid := false;
  end Initialize_Gimbals;

  procedure Update_Azimuth is
    P_Gain      : integer renames Azimuth_Proportional_Gain;
    D_Gain      : integer renames Azimuth_Derivative_Gain;
    Velocity    : integer;
    Error_Delta : RCS_Error;
  begin
    if Azimuth.Previous_Error_Valid then
      Error_Delta := Azimuth.Error - Azimuth.Previous_Error;
      Velocity    := (integer(Azimuth.Error) * P_Gain + integer(Error_Delta) * D_Gain) / 1024;

      -- Update Azimuth Position --
      if integer(Azimuth.Position) + Velocity > integer(RCS_Position'Last) then
        Azimuth.Position := RCS_Position'Last;
      elsif integer(Azimuth.Position) + Velocity < integer(RCS_Position'First) then
        Azimuth.Position := RCS_Position'First;
      else
        Azimuth.Position := RCS_Position(integer(Azimuth.Position) + Velocity);
      end if;
    else
      Azimuth.Previous_Error_Valid := true;
    end if;
    Azimuth.Previous_Error := Azimuth.Error;
  end Update_Azimuth;

  procedure Update_Altitude is
    P_Gain      : integer renames Altitude_Proportional_Gain;
    D_Gain      : integer renames Altitude_Derivative_Gain;
    Velocity    : integer;
    Error_Delta : RCS_Error;
  begin
    if Altitude.Previous_Error_Valid then
      Error_Delta := Altitude.Error - Altitude.Previous_Error;
      Velocity    := (integer(Altitude.Error) * P_Gain + integer(Error_Delta) * D_Gain) / 1024;

      -- Update Altitude Position --
      if integer(Altitude.Position) + Velocity > integer(RCS_Position'Last) then
        Altitude.Position := RCS_Position'Last;
      elsif integer(Altitude.Position) + Velocity < integer(RCS_Position'First) then
        Altitude.Position := RCS_Position'First;
      else
        Altitude.Position := RCS_Position(integer(Altitude.Position) + Velocity);
      end if;
    else
      Altitude.Previous_Error_Valid := true;
    end if;
    Altitude.Previous_Error := Altitude.Error;
  end Update_Altitude;

begin

  put_line("+ Pixy Tracking Demo Started +");

  Initialize_Gimbals;

  Pixy_Init_Status := Pixy.Init;
  Frame_Index      := 0;

  -- Was there an error initializing Pixy? --
  if Pixy_Init_Status /= 0 then
    put("Error: pixy_init() [" & int'image(Pixy_Init_Status) & "] ");
    Pixy.Error(Pixy_Init_Status);
    return Pixy_Init_Status;
  end if;

  Tracking_Loop:
  while not SIGINT loop

    -- Wait for new blocks to be available --
    Waiting_Loop:
    while Pixy.Blocks_Are_New = 0 and not SIGINT loop
      null;
    end loop Waiting_Loop;

    -- Get blocks from Pixy --
    blocks_copied := Get_Blocks(1, Block'access);

    if blocks_copied < 0 then
      -- Error: Pixy.Get_Blocks --
      put("Error: pixy_get_blocks() [" & int'image(blocks_copied) & "]");
      Pixy.Error(blocks_copied);
    end if;

    if blocks_copied > 0 then

      -- Calculate the difference between the center of Pixy's --
      -- focus and the target.                                 --
      Azimuth.Error  := RCS_Error(Pixy_X_Center) - RCS_Error(Block.X);
      Altitude.Error := RCS_Error(Block.Y) - RCS_Error(Pixy_Y_Center);

      -- Apply corrections to the Azimuth/Elevation with the goal --
      -- of putting the target in the center of Pixy's focus.     --
      Update_Azimuth;
      Update_Altitude;

      Result := RCS_Set_Position(RCS_Azimuth_Channel, uint16(Azimuth.Position));
      if Result < 0 then
        put("Error: pixy_rcs_set_position() [" & int'image(Result) & "]");
        Pixy.Error(Result);
      end if;

      Result := RCS_Set_Position(RCS_Altitude_Channel, uint16(Altitude.Position));
      if Result < 0 then
        put("Error: pixy_rcs_set_position() [" & int'image(Result) & "]");
        Pixy.Error(Result);
      end if;

      if Frame_Index mod 50 = 0 then
        put_line("frame " & Integer'image(Frame_Index) & ":");
        put_line("  sig: " & uint16'image(Block.Signature) &
                 "  x:"      & uint16'image(Block.X) &
                 "  y:"      & uint16'image(Block.Y) &
                 "  width:"  & uint16'image(Block.Width) &
                 "  height:" & uint16'image(Block.Height));
      end if;

      Frame_Index := Frame_Index + 1;
    end if;
  end loop Tracking_Loop;

  Pixy.Close;

  return 0;
end pan_tilt;
