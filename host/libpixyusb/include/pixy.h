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
#include "pixydefs.h"

// Pixy C API //

extern "C"
{
  struct Block
  {
    uint16_t signature;
    uint16_t x;
    uint16_t y;
    uint16_t width;
    uint16_t height;
  };

  /**
    @brief Creates a connection with Pixy and listens for Pixy messages.
  */
  void     pixy_init();

  /**
    @brief      Copies up to 'max_blocks' number of Blocks to the address pointed
                to by 'blocks'. 
    @param[in]  max_blocks Maximum number of Blocks to copy to the address pointed to
                           by 'blocks'.
    @param[out] blocks     Address of an array in which to copy the blocks to.
                           The array must be large enough to write 'max_blocks' number
                           of Blocks to.
    @return     Number of blocks copied.
  */
  uint16_t pixy_get_blocks(uint16_t max_blocks, struct Block * blocks);
  
  /**
    @brief Terminates connection with Pixy.
  */
  void     pixy_close();
}

#endif
