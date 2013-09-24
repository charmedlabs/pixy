#include "debug.h"

#ifdef CORE_M0
int fputc(int c, FILE *f) 
{
	UARTPutChar((LPC_USARTn_Type *)LPC_UART1, (uint8_t)c); 
	return (c);
}
#endif
