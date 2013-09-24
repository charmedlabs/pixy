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
