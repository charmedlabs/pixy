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

  int pixy_command(const char *name, ...)
  {
    va_list arguments;
    int     return_value;
    
    va_start(arguments, name);
    return_value = interpreter.send_command(name, arguments);
    va_end(arguments);
    
    return return_value;
  }

  uint16_t pixy_get_blocks(uint16_t max_blocks, struct Block * blocks)
  {
    return interpreter.get_blocks(max_blocks, blocks);
  }
}
