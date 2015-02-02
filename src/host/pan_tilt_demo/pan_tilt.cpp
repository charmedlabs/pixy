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
#define PAN_PROPORTIONAL_GAIN     400
#define PAN_DERIVATIVE_GAIN       300
#define TILT_PROPORTIONAL_GAIN    500
#define TILT_DERIVATIVE_GAIN      400

// Pixy Block Buffer //
struct Block  blocks [BLOCK_BUFFER_SIZE];

static bool run_flag = true;

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

  run_flag = false;
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
  char buf[128];
  int res, i=0;

  // Catch CTRL+C (SIGINT) signals //
  signal(SIGINT, handle_SIGINT);

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

 
  while(run_flag) {
    int32_t pan_error;
    int32_t tilt_error;
    int     blocks_copied;

    // Wait for new blocks to be available //

    while(!pixy_blocks_are_new());

    // Get blocks from Pixy //

    blocks_copied = pixy_get_blocks(BLOCK_BUFFER_SIZE, &blocks[0]);

    if(blocks_copied < 0) {
      // Error: pixy_get_blocks //
      printf("pixy_get_blocks(): ");
      pixy_error(blocks_copied);
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

#if 1
    res = pixy_rcs_set_position(RCS_PAN_CHANNEL, pan.position);
    if (res<0)
      printf("pan position error %d\n", res);
    res = pixy_rcs_set_position(RCS_TILT_CHANNEL, tilt.position);
    if (res<0)
      printf("tilt position error %d\n", res);
#endif    
    blocks[0].print(buf);
    printf("%d: %s", i++, buf);
  }
  pixy_close();
}
