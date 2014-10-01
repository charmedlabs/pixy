%module pixy

%{
#define SWIG_FILE_WITH_INIT
#include "../libpixyusb/include/pixy.h"
%}

int pixy_init();
int pixy_get_blocks(uint16_t max_blocks, struct Block *blocks);
