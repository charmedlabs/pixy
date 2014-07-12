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
#include <string.h>
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

  // Request Pixy firmware version //
  {
    uint16_t * pixy_version;
    uint32_t   version_length;
    uint32_t   response;
    uint16_t   version[3];
    int        return_value;

    return_value = pixy_command("version",  0, &response, &version_length, &pixy_version, 0);

    if (return_value) {
      // Error //
      printf("Failed to retrieve Pixy firmware version. Error: %d\n", return_value);
    } else {
      // Success //
      memcpy((void *) version, pixy_version, 3 * sizeof(uint16_t));
      printf("Pixy Firmware Version: %d.%d.%d\n", version[0], version[1], version[2]);
    }
  }

  for(;;)
  {
    // Get blocks from Pixy //
    blocks_copied = pixy_get_blocks(BLOCK_BUFFER_SIZE, &blocks[0]);

    // Display received blocks //
    for(index = 0; index != blocks_copied; ++index) {

      switch (blocks[index].type) {
        case TYPE_NORMAL:
          printf("[sig:%2u w:%3u h:%3u x:%3u y:%3u]\n",
                 blocks[index].signature,
                 blocks[index].width,
                 blocks[index].height,
                 blocks[index].x,
                 blocks[index].y);
        break;

        case TYPE_COLOR_CODE:
          printf("[sig:%2u w:%3u h:%3u x:%3u y:%3u ang:%3i]\n",
                 blocks[index].signature,
                 blocks[index].width,
                 blocks[index].height,
                 blocks[index].x,
                 blocks[index].y,
                 blocks[index].angle);
        break;
      }
    }

    // Sleep for 1/10 sec //
    usleep(100000);
  }
}
