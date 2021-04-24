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

#include <debug.h>
#include <chirp.h>
#include <cycletimer.h>
#include <pixyvals.h>
#include "exec_m0.h"
#include "frame_m0.h"
#include "rls_m0.h"

int main(void)
{
    chirpOpen();
    exec_init();
    frame_init();
    rls_init();
    exec_loop();
    return 0;
}
