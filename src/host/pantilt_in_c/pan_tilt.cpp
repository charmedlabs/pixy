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

#define BLOCK_BUFFER_SIZE          25

#define PIXY_X_CENTER              ((PIXY_MAX_X-PIXY_MIN_X)/2)
#define PIXY_Y_CENTER              ((PIXY_MAX_Y-PIXY_MIN_Y)/2)

#define PIXY_RCS_PAN_CHANNEL        0
#define PIXY_RCS_TILT_CHANNEL       1

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

struct Gimbal pan;
struct Gimbal tilt;

void handle_SIGINT(int unused)
{
  // On CTRL+C - abort! //

  run_flag = false;
}

void initialize_gimbals()
{
  pan.position           = PIXY_RCS_CENTER_POS;
  pan.previous_error     = 0x80000000L;
  pan.proportional_gain  = PAN_PROPORTIONAL_GAIN;
  pan.derivative_gain    = PAN_DERIVATIVE_GAIN;
  tilt.position          = PIXY_RCS_CENTER_POS;
  tilt.previous_error    = 0x80000000L;
  tilt.proportional_gain = TILT_PROPORTIONAL_GAIN;
  tilt.derivative_gain   = TILT_DERIVATIVE_GAIN;
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

    if (gimbal->position > PIXY_RCS_MAX_POS) {
      gimbal->position = PIXY_RCS_MAX_POS;
    } else if (gimbal->position < PIXY_RCS_MIN_POS) {
      gimbal->position = PIXY_RCS_MIN_POS;
    }
  }

  gimbal->previous_error = error;
}

int main(int argc, char *  argv[])
{
  int     pixy_init_status;
  char    buf[128];
  int     frame_index = 0;
  int     result;
  int     pan_error;
  int     tilt_error;
  int     blocks_copied;
  int     index;


  printf("+ Pixy Tracking Demo Started +\n");
  fflush(stdout);

  initialize_gimbals();

  // Catch CTRL+C (SIGINT) signals //
  signal(SIGINT, handle_SIGINT);

  // Connect to Pixy //
  pixy_init_status = pixy_init();

  // Was there an error initializing pixy? //
  if(pixy_init_status != 0)
  {
    // Error initializing Pixy //
    printf("Error: pixy_init() [%d] ", pixy_init_status);
    pixy_error(pixy_init_status);

    return pixy_init_status;
  }

  while(run_flag) {

    // Wait for new blocks to be available //
    while(!pixy_blocks_are_new() && run_flag);

    // Get blocks from Pixy //

    blocks_copied = pixy_get_blocks(BLOCK_BUFFER_SIZE, &blocks[0]);

    if(blocks_copied < 0) {
      // Error: pixy_get_blocks //
      printf("Error: pixy_get_blocks() [%d] ", blocks_copied);
      pixy_error(blocks_copied);
      fflush(stdout);
    }

    if (blocks_copied>0) {
      // Calculate the difference between the   //
      // center of Pixy's focus and the target. //

      pan_error  = PIXY_X_CENTER - blocks[0].x;
      tilt_error = blocks[0].y - PIXY_Y_CENTER;

      // Apply corrections to the pan/tilt with the goal //
      // of putting the target in the center of          //
      // Pixy's focus.                                   //

      gimbal_update(&pan, pan_error);
      gimbal_update(&tilt, tilt_error);

      result = pixy_rcs_set_position(PIXY_RCS_PAN_CHANNEL, pan.position);
      if (result < 0) {
        printf("Error: pixy_rcs_set_position() [%d] ", result);
        pixy_error(result);
        fflush(stdout);
      }

      result = pixy_rcs_set_position(PIXY_RCS_TILT_CHANNEL, tilt.position);
      if (result<0) {
        printf("Error: pixy_rcs_set_position() [%d] ", result);
        pixy_error(result);
        fflush(stdout);
      }
    }

    if(frame_index % 50 == 0) {
      // Display received blocks //
      printf("frame %d:\n", frame_index);
      for(index = 0; index != blocks_copied; ++index) {
        printf("  sig:%2d x:%4d y:%4d width:%4d height:%4d\n",
               blocks[index].signature,
               blocks[index].x,
               blocks[index].y,
               blocks[index].width,
               blocks[index].height);
      }
      fflush(stdout);
    }

    frame_index++;
  }
  pixy_close();

  return 0;
}

// LEIMON 2015 //
