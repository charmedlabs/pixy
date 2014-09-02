#include "pixy.h"
#include "pixyinterpreter.hpp"

PixyInterpreter interpreter;

// Pixy C API //

extern "C" 
{
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

  void pixy_close()
  {
    if(!pixy_initialized) return;

    interpreter.close();
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

  uint16_t pixy_get_blocks(uint16_t max_blocks, struct Block * blocks)
  {
    return interpreter.get_blocks(max_blocks, blocks);
  }
}
