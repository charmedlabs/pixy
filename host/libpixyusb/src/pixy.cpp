#include "pixy.h"
#include "pixyinterpreter.hpp"

PixyInterpreter interpreter;

// Pixy C API //

extern "C" 
{
 void pixy_init()
  {
    interpreter.init();
  }

  void pixy_close()
  {
    interpreter.close();
  }

  uint16_t pixy_get_blocks(uint16_t max_blocks, struct Block * blocks)
  {
    return interpreter.get_blocks(max_blocks, blocks);
  }
}
