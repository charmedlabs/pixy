#include <stdio.h>
#include "pixy.h"
#include "pixyinterpreter.hpp"

PixyInterpreter interpreter;

// Pixy C API //

extern "C" 
{
  static struct
  {
    int           error;
    const char *  text;
  } PIXY_ERROR_TABLE[] = {
    { 0,                          "Success" },
    { PIXY_ERROR_USB_IO,          "USB Error: I/O" },
    { PIXY_ERROR_USB_BUSY,        "USB Error: Busy" },
    { PIXY_ERROR_USB_NO_DEVICE,   "USB Error: No device" },
    { PIXY_ERROR_USB_NOT_FOUND,   "USB Error: Target not found" },
    { PIXY_ERROR_CHIRP,           "Chirp Protocol Error" },
    { PIXY_ERROR_INVALID_COMMAND, "Pixy Error: Invalid command" },
    { 0,                          0 }
  };

  static int pixy_initialized = false;

  int pixy_init()
  {
    int return_value;

    return_value = interpreter.init();

    if(return_value == 0) 
    {
      pixy_initialized = true;
    }

    return return_value;
  }

  int pixy_get_blocks(uint16_t max_blocks, struct Block * blocks)
  {
    return interpreter.get_blocks(max_blocks, blocks);
  }

  int pixy_command(const char *name, ...)
  {
    va_list arguments;
    int     return_value;

    if(!pixy_initialized) return -1;

    va_start(arguments, name);
    return_value = interpreter.send_command(name, arguments);
    va_end(arguments);

    return return_value;
  }

  void pixy_close()
  {
    if(!pixy_initialized) return;

    interpreter.close();
  }

  void pixy_error(int error_code)
  {
    int index;

    // Convert pixy error code to string and display to stdout //

    index = 0;

    while(PIXY_ERROR_TABLE[index].text != 0) {

      if(PIXY_ERROR_TABLE[index].error == error_code) {
        printf("%s\n", PIXY_ERROR_TABLE[index].text);
        return;
      }

      index += 1;
    }

    printf("Undefined error: [%d]\n", error_code);
  }

  int pixy_led_set_RGB(uint8_t red, uint8_t green, uint8_t blue)
  {
    int      chirp_response;
    int      return_value;
    uint32_t RGB;

    // Pack the RGB value //
    RGB = blue + (green << 8) + (red << 16);

    return_value = pixy_command("led_set", CRP_INT32, RGB, END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    return return_value;
  }

  int pixy_led_set_max_current(uint32_t current)
  {
    int chirp_response;
    int return_value;

    return_value = pixy_command("led_setMaxCurrent", CRP_INT32, current, END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    return return_value;
  }

  int pixy_led_get_max_current()
  {
    int      return_value;
    uint32_t chirp_response;

    return_value = pixy_command("led_getMaxCurrent", END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    if (return_value < 0) {
      // Error //
      return return_value;
    } else {
      // Success //
      return chirp_response;
    }
  }

  int pixy_cam_set_auto_white_balance(uint8_t enable)
  {
    int      return_value;
    uint32_t chirp_response;

    return_value = pixy_command("cam_setAWB", CRP_UINT8, enable, END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    return return_value;
  }

  int pixy_cam_get_auto_white_balance()
  {
    int      return_value;
    uint32_t chirp_response;

    return_value = pixy_command("cam_getAWB", END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    if (return_value < 0) {
      // Error //
      return return_value;
    } else {
      // Success //
      return chirp_response;
    }
  }

  uint32_t pixy_cam_get_white_balance_value()
  {
    int      return_value;
    uint32_t chirp_response;

    return_value = pixy_command("cam_getWBV", END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    return chirp_response;
  }

  int pixy_cam_set_white_balance_value(uint8_t red, uint8_t green, uint8_t blue)
  {
    int      return_value;
    uint32_t chirp_response;
    uint32_t white_balance;

    white_balance = green + (red << 8) + (blue << 16);

    return_value = pixy_command("cam_setAWB", CRP_UINT32, white_balance, END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    return return_value;
  }

  int pixy_cam_set_auto_exposure_compensation(uint8_t enable)
  {
    int      return_value;
    uint32_t chirp_response;

    return_value = pixy_command("cam_setAEC", CRP_UINT8, enable, END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    return return_value;
}
  
  int pixy_cam_get_auto_exposure_compensation()
  {
    int      return_value;
    uint32_t chirp_response;

    return_value = pixy_command("cam_getAEC", END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    if (return_value < 0) {
      // Error //
      return return_value;
    } else {
      // Success //
      return chirp_response;
    }
  }

  int pixy_cam_set_exposure_compensation(uint8_t gain, uint16_t compensation)
  {
    int      return_value;
    uint32_t chirp_response;
    uint32_t exposure;

    exposure = gain + (compensation << 8);

    return_value = pixy_command("cam_setECV", CRP_UINT32, exposure, END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    return return_value;
  }

  int pixy_cam_get_exposure_compensation(uint8_t * gain, uint16_t * compensation)
  {
    uint32_t exposure;
    int      return_value;

    return_value = pixy_command("cam_getECV", END_OUT_ARGS, &exposure, END_IN_ARGS);

    if (return_value < 0) {
      // Chirp error //
      return return_value;
    }

    if(gain == 0 || compensation == 0) {
      // Error: Null pointer //
      return PIXY_ERROR_INVALID_PARAMETER;
    }

    printf("exp:%08x\n", exposure);

    *gain         = exposure & 0xFF;
    *compensation = 0xFFFF & (exposure >> 8);

    return 0;
  }

  int pixy_cam_set_brightness(uint8_t brightness)
  {
    int chirp_response;
    int return_value;

    return_value = pixy_command("cam_setBrightness", CRP_INT8, brightness, END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    return return_value;
  }

  int pixy_cam_get_brightness()
  {
    int chirp_response;
    int return_value;

    return_value = pixy_command("cam_getBrightness", END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    if (return_value < 0) {
      // Error //
      return return_value;
    } else {
      // Success //
      return chirp_response;
    }
  }

  int pixy_rcs_get_position(uint8_t channel)
  {
    int chirp_response;
    int return_value;

    return_value = pixy_command("rcs_getPos", CRP_INT8, channel, END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    if (return_value < 0) {
      // Error //
      return return_value;
    } else {
      // Success //
      return chirp_response;
    }
  }

  int pixy_rcs_set_position(uint8_t channel, uint16_t position)
  {
    int chirp_response;
    int return_value;

    if(channel > 1 || position > 999) {
      return PIXY_ERROR_INVALID_PARAMETER;
    }   

    return_value = pixy_command("rcs_setPos", CRP_INT8, channel, CRP_INT16, position, END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    return return_value;
  }

  int pixy_rcs_set_frequency(uint16_t frequency)
  {
    int chirp_response;
    int return_value;

    if(frequency < 20 || frequency > 300) {
      return PIXY_ERROR_INVALID_PARAMETER;
    }

    return_value = pixy_command("rcs_setFreq", CRP_INT16, frequency, END_OUT_ARGS, &chirp_response, END_IN_ARGS);

    return return_value;
  }

  int pixy_get_firmware_version(uint16_t * major, uint16_t * minor, uint16_t * build)
  {
    uint16_t * pixy_version;
    uint32_t   version_length;
    uint32_t   response;
    uint16_t   version[3];
    int        return_value;
    int        chirp_response;

    if(major == 0 || minor == 0 || build == 0) {
      // Error: Null pointer //
      return PIXY_ERROR_INVALID_PARAMETER;
    }

    return_value = pixy_command("version",  END_OUT_ARGS, &response, &version_length, &pixy_version, END_IN_ARGS);

    if (return_value < 0) {
      // Error //
      return return_value;
    }

    memcpy((void *) version, pixy_version, 3 * sizeof(uint16_t));

    *major = version[0];
    *minor = version[1];
    *build = version[2];

    return 0;
  }
}
