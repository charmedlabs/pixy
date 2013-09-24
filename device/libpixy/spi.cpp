#include "lpc43xx_ssp.h"
#include "misc.h"
#include "spi.h"

struct
{
	uint16_t *m_buf;
	uint32_t m_read;
	uint32_t m_write;
	uint32_t m_produced;
	uint32_t m_consumed;
}
g_receive;


struct
{
	uint16_t *m_buf;
	uint32_t m_read;
	uint32_t m_len;
	TransmitCallback m_callback;
}
g_transmit;


int spi_checkIdle()
{
	uint32_t i;
	// 2000, 120us
	// 1000, 60us
	LPC_GPIO_PORT->PIN[5] |= 0x20; // negate SPI_SS
	for (i=0; i<150; i++) // 9us
	{
		if (LPC_GPIO_PORT->PIN[5]&0x04)
			break;
	}
	if (i==150)
	{
		LPC_GPIO_PORT->PIN[5] &= ~0x20; // assert SPI_SS
		for (i=0; i<16; i++) // 1us
		{
			if (LPC_GPIO_PORT->PIN[5]&0x04)
				break;
		}
		if (i==16)
			return 1;
 	}
	LPC_GPIO_PORT->PIN[5] |= 0x20; // negate SPI_SS
	return 0;
}

int spi_sync()
{
	uint32_t timer;

	setTimer(&timer);
	while(1)
	{
		if(spi_checkIdle())
			return 1;
		if (getTimer(timer)>500000) // timeout .5 seconds
			return 0;
	}	
}

extern "C" void SSP1_IRQHandler(void);


void SSP1_IRQHandler(void)
{
	uint32_t d, len; 

	// toggle SPI_SS so we can receive the next word
	LPC_GPIO_PORT->PIN[5] |= 0x20; // negate SPI_SS
	d = LPC_SSP1->DR; // grab data
	LPC_GPIO_PORT->PIN[5] &= ~0x20; // assert SPI_SS


	while((LPC_SSP1->SR&SSP_SR_TNF) && g_transmit.m_len)
	{
		LPC_SSP1->DR = g_transmit.m_buf[g_transmit.m_read++];
		g_transmit.m_len--;
	}
	
	if (g_transmit.m_callback && g_transmit.m_len==0)
	{
		len = (*g_transmit.m_callback)(g_transmit.m_buf, SPI_TRANSMITBUF_SIZE);
		g_transmit.m_len = len;
		g_transmit.m_read = 0;
	}
#if 1
	while((LPC_SSP1->SR&SSP_SR_TNF) && g_transmit.m_len)
	{
		LPC_SSP1->DR = g_transmit.m_buf[g_transmit.m_read++];
		g_transmit.m_len--;
	}
#endif

	// receive data
	if (RECEIVE_LEN()<SPI_RECEIVEBUF_SIZE)
	{
		g_receive.m_buf[g_receive.m_write++] = d;
		g_receive.m_produced++;

		if (g_receive.m_write==SPI_RECEIVEBUF_SIZE)
			g_receive.m_write = 0;
	}
	
	// clear interrupt
	LPC_SSP1->DR = SSP_INTCFG_RT;  
}

int spi_receive(uint16_t *buf, uint32_t len)
{
	uint32_t i, recvlen;

	recvlen = RECEIVE_LEN();

	if (recvlen>0)
	{
		for (i=0; i<len && i<recvlen; i++)
		{
			buf[i] = g_receive.m_buf[g_receive.m_read++];
			g_receive.m_consumed++;

			if (g_receive.m_read==SPI_RECEIVEBUF_SIZE)
				g_receive.m_read = 0;
		}
		return i;	
	}
	return 0;
}


void spi_setCallback(TransmitCallback callback)
{
	g_transmit.m_callback = callback;
}
	
void spi_init()
{
	uint32_t i;
	volatile uint32_t d;
	SSP_CFG_Type configStruct;

	g_receive.m_buf = new uint16_t[SPI_RECEIVEBUF_SIZE];
	g_receive.m_read = 0;
	g_receive.m_write = 0;
	g_receive.m_produced = 0;
	g_receive.m_consumed = 0;

	g_transmit.m_buf = new uint16_t[SPI_TRANSMITBUF_SIZE];
	g_transmit.m_read = 0;
	g_transmit.m_len = 0;

	g_transmit.m_callback = (TransmitCallback)NULL;

	configStruct.CPHA = SSP_CPHA_FIRST;
	configStruct.CPOL = SSP_CPOL_HI;
	configStruct.ClockRate = 204000000;
	configStruct.Databit = SSP_DATABIT_16;
	configStruct.Mode = SSP_SLAVE_MODE;
	configStruct.FrameFormat = SSP_FRAME_SPI;

	// Initialize SSP peripheral with parameter given in structure above
	SSP_Init(LPC_SSP1, &configStruct);

	// clear receive fifo
	for (i=0; i<8; i++)
		d = LPC_SSP1->DR;

	// Enable SSP peripheral
	SSP_Cmd(LPC_SSP1, ENABLE);
		
	SSP_ClearIntPending(LPC_SSP1, SSP_INTCFG_RT);
	SSP_IntConfig(LPC_SSP1, SSP_INTCFG_RT, ENABLE);

	// sync
	spi_sync();					

	// enable interrupt
	NVIC_SetPriority(SSP1_IRQn, 0);	// high priority interrupt
	NVIC_EnableIRQ(SSP1_IRQn);

}
