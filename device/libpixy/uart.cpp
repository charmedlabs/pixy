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
 	static uint8_t sc = 0;

	m_flag = false;

	/* Determine the interrupt source */
	status = m_uart->IIR & UART_IIR_INTID_MASK;

	// Receive Data Available 
	if (status==UART_IIR_INTID_RDA) 
	{
		m_rq.write(m_uart->RBR&UART_RBR_MASKBIT);
	}
	else if (status==UART_IIR_INTID_CTI)
		v = m_uart->RBR;
	// Transmit Holding Empty
	else if (status==UART_IIR_INTID_THRE)
	{
		for (i=0; i<16; i++)
		{
#if 1
			if (m_tq.read(&c))
			{
				m_flag = true;
				m_uart->THR = c;
			}
#else
		m_flag = true;
		m_uart->THR = sc++;	
#endif
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
		m_uart->THR = 0; // send a 0 to get the transmit interrupt going again		
		m_uart->THR = 0; 		
	}
	return 0;
}


Uart::Uart(LPC_USARTn_Type *uart,  SerialCallback callback) : m_rq(UART_RECEIVE_BUF_SIZE), m_tq(UART_TRANSMIT_BUF_SIZE, callback)
{
	m_uart = uart;
	m_flag = false;
	 
	UART_CFG_Type ucfg;
	UART_FIFO_CFG_Type ufifo;

	ucfg.Baud_rate = 19200;
	ucfg.Databits = UART_DATABIT_8;
	ucfg.Parity = UART_PARITY_NONE;
	ucfg.Stopbits = UART_STOPBIT_1;
	ucfg.Clock_Speed = CLKFREQ;

	ufifo.FIFO_DMAMode = DISABLE;
	ufifo.FIFO_Level = UART_FIFO_TRGLEV0;
	ufifo.FIFO_ResetRxBuf = ENABLE;
	ufifo.FIFO_ResetTxBuf = ENABLE;

	UART_Init(m_uart, &ucfg);
	UART_FIFOConfig(m_uart, &ufifo);
	UART_TxCmd(m_uart, ENABLE);

	UART_IntConfig(m_uart, UART_INTCFG_RBR, ENABLE);
	UART_IntConfig(m_uart, UART_INTCFG_THRE, ENABLE);

    NVIC_SetPriority(USART0_IRQn, 0);
}


void uart_init(SerialCallback callback)
{
	g_uart0 = new Uart(LPC_USART0, callback);
}
