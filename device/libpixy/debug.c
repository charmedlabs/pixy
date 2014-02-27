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

#include "debug.h"

#ifdef CORE_M0
int fputc(int c, FILE *f) 
{
	UARTPutChar((LPC_USARTn_Type *)LPC_UART1, (uint8_t)c); 
	return (c);
}
#endif
