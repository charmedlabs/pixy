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

#include "inttypes.h"
#include "lpc43xx_scu.h"
#include "pixyvals.h"
#include "uart.h"

Uart *g_uart0;

extern "C" void UART0_IRQHandler(void);

void UART0_IRQHandler(void)
{
	g_uart0->irqHandler();
}

void Uart::irqHandler()
{
	uint32_t status;
	uint8_t i, c;
	volatile uint32_t v;

	m_flag = false;

	/* Determine the interrupt source */
	status = m_uart->IIR & UART_IIR_INTID_MASK;

	if (status==UART_IIR_INTID_RDA) // Receive Data Available 
		m_rq.write(m_uart->RBR&UART_RBR_MASKBIT);
	else if (status==UART_IIR_INTID_CTI)
		v = m_uart->RBR; // toss...
	else if (status==UART_IIR_INTID_THRE) // Transmit Holding Empty
	{
		for (i=0; i<UART_TX_FIFO_SIZE; i++) // fill transmit FIFO
		{
			if (m_tq.read(&c))
			{
				m_flag = true;
				m_uart->THR = c;
			}
		}
	}
}

int Uart::open()
{
	scu_pinmux(0x2, 0, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC1); 	         // U0_TXD 
	scu_pinmux(0x2, 1, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC1); 	         // U0_RXD
 	scu_pinmux(0x1, 3, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	         // turn SSP1_MISO into GPIO0[10]
	scu_pinmux(0x1, 4, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	         // turn SSP1_MOSI into GPIO0[11]

    NVIC_EnableIRQ(USART0_IRQn);
	return 0;
}

int Uart::close()
{
	scu_pinmux(0x2, 0, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // U0_TXD 
	scu_pinmux(0x2, 1, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         // U0_RXD

	NVIC_DisableIRQ(USART0_IRQn);
	return 0;
}

int Uart::receive(uint8_t *buf, uint32_t len)
{
	return 0;
}

int Uart::update()
{
	if (m_flag==false)
	{
		m_uart->THR = 0; // send a 0 to get the transmit interrupt going again, send 16 bits		
		m_uart->THR = 0; 		
	}
	return 0;
}


Uart::Uart(LPC_USARTn_Type *uart,  SerialCallback callback) : m_rq(UART_RECEIVE_BUF_SIZE), m_tq(UART_TRANSMIT_BUF_SIZE, callback)
{
	UART_FIFO_CFG_Type ufifo;
	UART_CFG_Type ucfg;

	m_uart = uart;
	m_flag = false;
	 	
	// regular config			 
	ucfg.Baud_rate = UART_DEFAULT_BAUDRATE;
	ucfg.Databits = UART_DATABIT_8;
	ucfg.Parity = UART_PARITY_NONE;
	ucfg.Stopbits = UART_STOPBIT_1;
	ucfg.Clock_Speed = CLKFREQ;

	UART_Init(m_uart, &ucfg);

	// config FIFOs
	ufifo.FIFO_DMAMode = DISABLE;
	ufifo.FIFO_Level = UART_FIFO_TRGLEV0;
	ufifo.FIFO_ResetRxBuf = ENABLE;
	ufifo.FIFO_ResetTxBuf = ENABLE;

	UART_FIFOConfig(m_uart, &ufifo);
	UART_TxCmd(m_uart, ENABLE);

	// enable interrupts
	UART_IntConfig(m_uart, UART_INTCFG_RBR, ENABLE);
	UART_IntConfig(m_uart, UART_INTCFG_THRE, ENABLE);

    NVIC_SetPriority(USART0_IRQn, 0);
}

int Uart::setBaudrate(uint32_t baudrate)
{
	UART_setBaudRate(m_uart, baudrate, CLKFREQ);
	return 0;
}

void uart_init(SerialCallback callback)
{
	g_uart0 = new Uart(LPC_USART0, callback);
}
