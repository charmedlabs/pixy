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

#define BLOCK_BUFFER_SIZE         25

#define PIXY_X_CENTER             160L
#define PIXY_Y_CENTER             100L
#define RCS_MIN_POSITION          0L
#define RCS_MAX_POSITION          1000L
#define RCS_CENTER_POSITION       500L

#define RCS_PAN_CHANNEL           0
#define RCS_TILT_CHANNEL          1

// PID control parameters //
#define PAN_PROPORTIONAL_GAIN     300
#define PAN_DERIVATIVE_GAIN       500
#define TILT_PROPORTIONAL_GAIN    500
#define TILT_DERIVATIVE_GAIN      700

// Pixy Block Buffer //
struct Block  blocks [BLOCK_BUFFER_SIZE];

struct Gimbal {
  int32_t position;
  int32_t previous_error;
  int32_t proportional_gain;
  int32_t derivative_gain;
};

// PID control variables //

struct Gimbal pan  = { .position          = RCS_CENTER_POSITION,
                       .previous_error    = 0x80000000L,
                       .proportional_gain = PAN_PROPORTIONAL_GAIN,
                       .derivative_gain   = PAN_DERIVATIVE_GAIN    };

struct Gimbal tilt = { .position          = RCS_CENTER_POSITION,
                       .previous_error    = 0x80000000L,
                       .proportional_gain = TILT_PROPORTIONAL_GAIN,
                       .derivative_gain   = TILT_DERIVATIVE_GAIN   };

void handle_SIGINT(int unused)
{
  // On CTRL+C - abort! //

  printf("\nBye!\n");
  exit(0);
}

void gimbal_update(struct Gimbal *  gimbal, int32_t error)
{
  long int velocity;
  int32_t  error_delta;
  int32_t  P_gain;
  int32_t  D_gain;

  if(gimbal->previous_error != 0x80000000L) {

    error_delta = error - gimbal->previous_error;
    P_gain      = gimbal->proportional_gain;
    D_gain      = gimbal->derivative_gain;

    /* Using the proportional and derivative gain for the gimbal,
       calculate the change to the position.  */
    velocity = (error * P_gain + error_delta * D_gain) >> 10;

    gimbal->position += velocity;

    if (gimbal->position > RCS_MAX_POSITION) {
      gimbal->position = RCS_MAX_POSITION;
    } else if (gimbal->position < RCS_MIN_POSITION) {
      gimbal->position = RCS_MIN_POSITION;
    }
  }

  gimbal->previous_error = error;
}

int main(int argc, char *  argv[])
{
  int pixy_init_status;

  // Connect to Pixy //
  pixy_init_status = pixy_init();

  // Was there an error initializing pixy? //
  if(!pixy_init_status == 0)
  {
    // Error initializing Pixy //
    printf("pixy_init(): ");
    pixy_error(pixy_init_status);

    return pixy_init_status;
  }

  // Request Pixy firmware version //
  {
    uint16_t major;
    uint16_t minor;
    uint16_t build;
    int      return_value;

    return_value = pixy_get_firmware_version(&major, &minor, &build);

    if (return_value) {
      // Error //
      printf("Failed to retrieve Pixy firmware version. ");
      pixy_error(return_value);

      return return_value;
    } else {
      // Success //
      printf(" Pixy Firmware Version: %d.%d.%d\n", major, minor, build);
    }
  }

  for(;;) {
    int32_t pan_error;
    int32_t tilt_error;
    int     blocks_copied;

    // Wait for new blocks to be available //

    while(!pixy_blocks_are_new()) {
      usleep(10000);
      printf(".");
    }

    // Get blocks from Pixy //

    blocks_copied = pixy_get_blocks(BLOCK_BUFFER_SIZE, &blocks[0]);

    if(blocks_copied < 0) {
      // Error: pixy_get_blocks //
      printf("pixy_get_blocks(): ");
      pixy_error(blocks_copied);
      usleep(250000);
    }

    // Calculate the difference between the   //
    // center of Pixy's focus and the target. //

    pan_error  = PIXY_X_CENTER - blocks[0].x;
    tilt_error = blocks[0].y - PIXY_Y_CENTER;

    // Apply corrections to the pan/tilt with the goal //
    // of putting the target in the center of          //
    // Pixy's focus.                                   //

    gimbal_update(&pan, pan_error);
    gimbal_update(&tilt, tilt_error);

    // XXX XXX DEBUG XXX XXX //
    // XXX XXX DEBUG XXX XXX //
    printf("p:%4d t:%4d -- pErr:%5d tErr:%5d\n",
           pan.position,
           tilt.position,
           pan_error,
           tilt_error);
    // XXX XXX DEBUG XXX XXX //
    // XXX XXX DEBUG XXX XXX //

    printf("[p:%d]\n", pixy_rcs_set_position(RCS_PAN_CHANNEL, pan.position));
    usleep(100000);
    printf("[t:%d]\n", pixy_rcs_set_position(RCS_TILT_CHANNEL, tilt.position));
    usleep(100000);
  }
}
