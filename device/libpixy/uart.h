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
