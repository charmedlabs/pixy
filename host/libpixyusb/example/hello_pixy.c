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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include "pixy.h"

#define BLOCK_BUFFER_SIZE    25

// Pixy Block buffer // 
struct Block blocks[BLOCK_BUFFER_SIZE];

void handle_SIGINT(int unused)
{
  // On CTRL+C - abort! //

  // Disconnect from Pixy //
  pixy_close();

  exit(0);
}

int main(int argc, char * argv[])
{
  int      index;
  uint16_t blocks_copied;

  // Catch CTRL+C (SIGINT) signals //
  signal(SIGINT, handle_SIGINT);

  printf("Hello Pixy: libpixyusb Version: %s\n", __LIBPIXY_VERSION__);

  // Connect to Pixy //
  pixy_init();

  for(;;)
  {
    // Get blocks from Pixy //
    blocks_copied = pixy_get_blocks(BLOCK_BUFFER_SIZE, &blocks[0]);

    // Display received blocks //
    for(index = 0; index != blocks_copied; ++index) {

      printf("[sig:%2u w:%3u h:%3u x:%3u y:%3u]\n",
             blocks[index].signature,
             blocks[index].width,
             blocks[index].height,
             blocks[index].x,
             blocks[index].y);
    }

    // Sleep for 1/10 sec //
    usleep(100000);
  }
}
