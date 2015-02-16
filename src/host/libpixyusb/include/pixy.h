//
// begin license header
//
// This file is part of Pixy CMUcam5 or "Pixy" for short
//
// All Pixy source code is provided under the terms of the
// GNU General Public License v2 (http://www.gnu.org/licenses/gpl-2.0.html).
// Those wishing to use Pixy source code, software and/or
// technologies under different licensing terms should contact us at
// cmucam@cs.cmu.edu. Such licensing terms are available for
// all portions of the Pixy codebase presented here.
//
// end license header
//

#ifndef __PIXY_H__
#define __PIXY_H__

#include <stdint.h>
#include <unistd.h>
#include <pixydefs.h>

// Pixy C API //

#ifdef __cplusplus
extern "C"
{
#endif

  #define PIXY_MAX_SIGNATURE          7

  // Pixy x-y position values
  #define PIXY_MIN_X                  0
  #define PIXY_MAX_X                  319
  #define PIXY_MIN_Y                  0
  #define PIXY_MAX_Y                  199

  // RC-servo values
  #define PIXY_RCS_MIN_POS            0
  #define PIXY_RCS_MAX_POS            1000
  #define PIXY_RCS_CENTER_POS         ((PIXY_RCS_MAX_POS-PIXY_RCS_MIN_POS)/2)

  // Block types
  #define PIXY_BLOCKTYPE_NORMAL       0
  #define PIXY_BLOCKTYPE_COLOR_CODE   1

  struct Block
  {
    void print(char *buf)
    {
      int i, j;
      char sig[6], d;
      bool flag;
      if (type==PIXY_BLOCKTYPE_COLOR_CODE)
      {
        // convert signature number to an octal string
        for (i=12, j=0, flag=false; i>=0; i-=3)
        {
          d = (signature>>i)&0x07;
          if (d>0 && !flag)
            flag = true;
          if (flag)
            sig[j++] = d + '0';
        }
        sig[j] = '\0';	
        sprintf(buf, "CC block! sig: %s (%d decimal) x: %d y: %d width: %d height: %d angle %d", sig, signature, x, y, width, height, angle);
      }
      else // regular block.  Note, angle is always zero, so no need to print
        sprintf(buf, "sig: %d x: %d y: %d width: %d height: %d", signature, x, y, width, height);		
    }

    uint16_t type;
    uint16_t signature;
    uint16_t x;
    uint16_t y;
    uint16_t width;
    uint16_t height;
    int16_t  angle;
  };

  /**
    @brief Creates a connection with Pixy and listens for Pixy messages.
    @return  0                         Success
    @return  PIXY_ERROR_USB_IO         USB Error: I/O
    @return  PIXY_ERROR_NOT_FOUND      USB Error: Pixy not found
    @return  PIXY_ERROR_USB_BUSY       USB Error: Busy
    @return  PIXY_ERROR_USB_NO_DEVICE  USB Error: No device
  */
  int pixy_init();

  /**
    @brief      Indicates when new block data from Pixy is received.

    @return  1  New Data:   Block data has been updated.
    @return  0  Stale Data: Block data has not changed since pixy_get_blocks() was
                            last called.
  */
  int pixy_blocks_are_new();

  /**
    @brief      Copies up to 'max_blocks' number of Blocks to the address pointed
                to by 'blocks'.
    @param[in]  max_blocks Maximum number of Blocks to copy to the address pointed to
                           by 'blocks'.
    @param[out] blocks     Address of an array in which to copy the blocks to.
                           The array must be large enough to write 'max_blocks' number
                           of Blocks to.
    @return  Non-negative                  Success: Number of blocks copied
    @return  PIXY_ERROR_USB_IO             USB Error: I/O
    @return  PIXY_ERROR_NOT_FOUND          USB Error: Pixy not found
    @return  PIXY_ERROR_USB_BUSY           USB Error: Busy
    @return  PIXY_ERROR_USB_NO_DEVICE      USB Error: No device
    @return  PIXY_ERROR_INVALID_PARAMETER  Invalid pararmeter specified
  */
  int pixy_get_blocks(uint16_t max_blocks, struct Block * blocks);

  /**
    @brief      Send a command to Pixy.
    @param[in]  name  Chirp remote procedure call identifier string.
    @return     -1    Error

  */
  int pixy_command(const char *name, ...);

  /**
    @brief Terminates connection with Pixy.
  */
  void pixy_close();

  /**
    @brief Send description of pixy error to stdout.
    @param[in] error_code  Pixy error code
  */
  void pixy_error(int error_code);

  /**
    @brief  Set color of pixy LED.
    @param[in] red   Brightness value for red LED element.   [0, 255] 0 = Off, 255 = On
    @param[in] green Brightness value for green LED element. [0, 255] 0 = Off, 255 = On
    @param[in] blue  Brightness value for blue LED element.  [0, 255] 0 = Off, 255 = On
    @return     0         Success
    @return     Negative  Error
  */
  int pixy_led_set_RGB(uint8_t red, uint8_t green, uint8_t blue);

  /**
    @brief  Set pixy LED maximum current.
    @param[in] current  Maximum current (microamps).
    @return     0         Success
    @return     Negative  Error
  */
  int pixy_led_set_max_current(uint32_t current);

  /**
    @brief   Get pixy LED maximum current.
    @return     Non-negative Maximum LED current value (microamps).
    @return     Negative     Error
  */
  int pixy_led_get_max_current();

  /**
    @brief    Enable or disable pixy camera auto white balance.
    @param    enable  1: Enable white balance.
                      0: Disable white balance.
    @return     0         Success
    @return     Negative  Error
  */
  int pixy_cam_set_auto_white_balance(uint8_t value);

  /**
    @brief    Get pixy camera auto white balance setting.
    @return     1         Auto white balance is enabled.
    @return     0         Auto white balance is disabled.
    @return     Negative  Error
  */
  int pixy_cam_get_auto_white_balance();

  /**
    @brief   Get pixy camera white balance()
    @return  Composite value for RGB white balance:
             white balance = green_value + (red_value << 8) + (blue << 16)
  */
  uint32_t pixy_cam_get_white_balance_value();

  /**
    @brief     Set pixy camera white balance.
    @param[in] red    Red white balance value.
    @param[in] green  Green white balance value.
    @param[in] blue   Blue white balance value.
    @return     0         Success
    @return     Negative  Error
  */
  int pixy_cam_set_white_balance_value(uint8_t red, uint8_t green, uint8_t blue);

  /**
    @brief     Enable or disable pixy camera auto exposure compensation.
    @param[in] enable  0: Disable auto exposure compensation.
                       1: Enable auto exposure compensation.
    @return     0         Success
    @return     Negative  Error
  */
  int pixy_cam_set_auto_exposure_compensation(uint8_t enable);

  /**
    @brief     Get pixy camera auto exposure compensation setting.
    @return     1         Auto exposure compensation enabled.
    @return     0         Auto exposure compensation disabled.
    @return     Negative  Error
  */
  int pixy_cam_get_auto_exposure_compensation();

  /**
    @brief     Set pixy camera exposure compensation.
    @param[in] gain  Camera gain.
    @param[in] comp  Camera exposure compensation.
    @return     0         Success
    @return     Negative  Error
  */
  int pixy_cam_set_exposure_compensation(uint8_t gain, uint16_t compensation);

  /**
    @brief     Get pixy camera exposure compensation.
    @param[out] gain  Camera gain.
    @param[out] comp  Camera exposure compensation.
    @return     0         Success
    @return     Negative  Error
  */
  int pixy_cam_get_exposure_compensation(uint8_t * gain, uint16_t * compensation);

  /**
    @brief     Set pixy camera brightness.
    @param[in] brightness  Brightness value.
    @return     0         Success
    @return     Negative  Error
  */
  int pixy_cam_set_brightness(uint8_t brightness);

  /**
    @brief     Get pixy camera brightness.
    @return     Non-negative Brightness value.
    @return     Negative     Error
  */
  int pixy_cam_get_brightness();

  /**
    @brief     Get pixy servo axis position.
    @param     channel  Channel value. Range: [0, 1]
    @return     Position of channel. Range: [0, 999]
    @return     Negative  Error
  */
  int pixy_rcs_get_position(uint8_t channel);

  /**
    @brief     Set pixy servo axis position.
    @param     channel  Channel value. Range: [0, 1]
    @param     position Position value of the channel. Range: [0, 999]
    @return      0         Success
    @return      Negative  Error
  */
  int pixy_rcs_set_position(uint8_t channel, uint16_t position);

  /**
    @brief     Set pixy servo pulse width modulation (PWM) frequency.
    @param     frequency Range: [20, 300] Hz Default: 50 Hz
  */
  int pixy_rcs_set_frequency(uint16_t frequency);

  /**
    @brief    Get pixy firmware version.
    @param[out]  major  Major version component
    @param[out]  minor  Minor version component
    @param[out]  build  Build identifier
    @return      0         Success
    @return      Negative  Error
  */
  int pixy_get_firmware_version(uint16_t * major, uint16_t * minor, uint16_t * build);


#ifdef __cplusplus
}
#endif

#endif
