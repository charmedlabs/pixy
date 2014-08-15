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

#ifndef _UART_H
#define _UART_H
#include "iserial.h"
#include "lpc43xx_uart.h"

#define UART_TRANSMIT_BUF_SIZE     32
#define UART_RECEIVE_BUF_SIZE      32
#define UART_DEFAULT_BAUDRATE      19200

class Uart : public Iserial
{
public:
	Uart(LPC_USARTn_Type *uart, SerialCallback callback);

	// Iserial methods
	virtual int open();
	virtual int close();
	virtual int receive(uint8_t *buf, uint32_t len);
	virtual int update();

	int setBaudrate(uint32_t baudrate);
	void irqHandler();

private:

	LPC_USARTn_Type *m_uart;	
	ReceiveQ<uint8_t> m_rq;
	TransmitQ<uint8_t> m_tq;
	bool m_flag;
};

void uart_init(SerialCallback callback);

extern Uart *g_uart0;
#endif
