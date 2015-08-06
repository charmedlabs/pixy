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

#ifndef CYCLETIMER_H
#define CYCLETIMER_H
#include "lpc_types.h"
#include "lpc43xx.h"

#define CTIMER_DECLARE() \
  uint32_t ct_start; \
  uint32_t ct_diff

#define CTIMER_START() \
  ct_start = LPC_TIMER1->TC

#define CTIMER_STOP() \
  ct_diff = LPC_TIMER1->TC-ct_start

#define CTIMER_GET() \
   ct_diff

#endif
