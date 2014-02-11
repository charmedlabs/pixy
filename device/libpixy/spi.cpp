#include "pixy_init.h"
#include "misc.h"
#include "spi.h"

Spi *g_spi = 0;

int Spi::checkIdle()
{
	uint32_t i;
	// 2000, 120us
	// 1000, 60us
	SS_NEGATE(); // negate SPI_SS
	for (i=0; i<150; i++) // 9us
	{
		if (LPC_GPIO_PORT->PIN[5]&0x04)
			break;
	}
	if (i==150)
	{
		SS_ASSERT(); // assert SPI_SS
		for (i=0; i<16; i++) // 1us
		{
			if (LPC_GPIO_PORT->PIN[5]&0x04)
				break;
		}
		if (i==16)
			return 1;
 	}
	SS_ASSERT(); // assert SPI_SS
	return 0;
}

int Spi::sync()
{
	uint32_t timer;
	int res = 0;

	SSP_IntConfig(LPC_SSP1, SSP_INTCFG_RX, DISABLE);

	setTimer(&timer);
	while(1)
	{
		if(checkIdle())
		{
			res = 1;
			break;
		}
		if (getTimer(timer)>500000) // timeout .5 seconds
			break;
	}

	SSP_IntConfig(LPC_SSP1, SSP_INTCFG_RX, ENABLE);
	return res;	
}

extern "C" void SSP1_IRQHandler(void);


void SSP1_IRQHandler(void)
{
	g_spi->slaveHandler();
}

void Spi::slaveHandler()
{
	uint32_t d;
	uint16_t d16; 

	// toggle SPI_SS so we can receive the next word
	SS_NEGATE(); // negate SPI_SS
	SS_ASSERT(); // assert SPI_SS

	d = LPC_SSP1->DR; // grab data
	// clear interrupt
	LPC_SSP1->ICR = SSP_INTCFG_RX;  

	// fill fifo
	while(LPC_SSP1->SR&SSP_SR_TNF) 
	{
		if (m_tq.read(&d16)==0)
			break;
		LPC_SSP1->DR = d16;
	}
	
	// receive data
	if ((d&SPI_SYNC_MASK)==SPI_SYNC_WORD)
	{
		m_rq.write(d);
		m_sync = true;
	}
	else
		m_sync = false;
}

int Spi::receive(uint8_t *buf, uint32_t len)
{
	uint32_t i;
	uint16_t *buf16 = (uint16_t *)buf;

	len /= 2;

	for (i=0; i<len; i++)
	{
		if (m_rq.read(buf16+i)==0)
			break;
	}

	return i*2;
}

int Spi::open()
{
	// todo: setup pin muxes
	return 0;
}

int Spi::close()
{
	return 0;
}

int Spi::update()
{
	uint16_t recvBuf[1];

	// just grab a word-- don't worry about looking at data except to see if we're synchronized.
	if (receive((uint8_t *)recvBuf, 2))
	{
		if (!m_sync) // if received data isn't correct, we're out of sync
		{
			m_syncCounter++;

			if (m_syncCounter==SPI_MIN_SYNC_COUNT) // if we receive enough bad syncs in a row, we need to resync 
			{
				sync();
				cprintf("sync\n");
				m_syncCounter = 0;
			}
		}
		else
			m_syncCounter = 0;
	}
	else
	{
		// need to pump up the fifo because we only get an interrupt when fifo is half full
		// (and we won't receive data if we don't toggle SS)
		SS_NEGATE();
		SS_ASSERT();
	}
	return 0;
}
	
void spi_init(SerialCallback callback)
{
	g_spi = new Spi(callback);
}

Spi::Spi(SerialCallback callback) : m_rq(SPI_RECEIVEBUF_SIZE), m_tq(SPI_TRANSMITBUF_SIZE, callback)
{
	uint32_t i;
	volatile uint32_t d;
	SSP_CFG_Type configStruct;

	// configure SGPIO bit as output so we can toggle slave select (SS)
	LPC_SGPIO->OUT_MUX_CFG14 = 4;
	LPC_SGPIO->GPIO_OENREG = 1<<14;

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
		
	SSP_ClearIntPending(LPC_SSP1, SSP_INTCFG_RX);
	SSP_IntConfig(LPC_SSP1, SSP_INTCFG_RX, ENABLE);

	// enable interrupt
	NVIC_SetPriority(SSP1_IRQn, 0);	// high priority interrupt
	NVIC_EnableIRQ(SSP1_IRQn);

	m_sync = false;
	m_syncCounter = 0;

	// sync
	sync();					
}
