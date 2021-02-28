%module pixy

%include "stdint.i"
%include "carrays.i"
%include "cdata.i"
%include "typemaps.i"

%{
#define SWIG_FILE_WITH_INIT
#include "pixy.h"
#include "helper_commands.h"
%}

%array_class(struct Block, BlockArray);

// Expose a function to create a uint8_t array in Python
%array_class(unsigned char, byteArray);
%include "helper_commands.h"

// Define these as output arguments
int pixy_cam_get_exposure_compensation(uint8_t *OUTPUT, uint16_t *OUTPUT);
int pixy_get_firmware_version(uint16_t *OUTPUT, uint16_t *OUTPUT, uint16_t *OUTPUT);


int pixy_init();
int pixy_command(const char *name, ...);
int pixy_get_blocks(uint16_t max_blocks, BlockArray *blocks);
int pixy_led_set_RGB(uint8_t red, uint8_t green, uint8_t blue);
int pixy_led_set_max_current(uint32_t current);
int pixy_led_get_max_current();
int pixy_cam_set_auto_white_balance(uint8_t value);
int pixy_cam_get_auto_white_balance();
uint32_t pixy_cam_get_white_balance_value();
int pixy_cam_set_white_balance_value(uint8_t red, uint8_t green, uint8_t blue);
int pixy_cam_set_auto_exposure_compensation(uint8_t enable);
int pixy_cam_get_auto_exposure_compensation();
int pixy_cam_set_exposure_compensation(uint8_t gain, uint16_t compensation);
int pixy_cam_get_exposure_compensation(uint8_t *gain, uint16_t *compensation);
int pixy_cam_set_brightness(uint8_t brightness);
int pixy_cam_get_brightness();
int pixy_get_firmware_version(uint16_t *major, uint16_t *minor, uint16_t *build);
void pixy_close();

struct Block
{
  uint16_t type;
  uint16_t signature;
  uint16_t x;
  uint16_t y;
  uint16_t width;
  uint16_t height;
  int16_t  angle;
};
