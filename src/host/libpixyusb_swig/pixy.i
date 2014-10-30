%module pixy

%include "stdint.i"

%{
#define SWIG_FILE_WITH_INIT
#include "pixy.h"
%}

int pixy_init();
int pixy_get_blocks(uint16_t max_blocks, struct Block *blocks);

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
