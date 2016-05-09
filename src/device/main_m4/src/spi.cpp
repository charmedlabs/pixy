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

#include "pixy_init.h"
#include "misc.h"
#include "spi.h"
#include "lpc43xx_scu.h"

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

	if (!m_autoSlaveSelect)
		return 0;

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

	if (m_autoSlaveSelect)
	{
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
				d16 = 0; // stuff fifo with 0s
			LPC_SSP1->DR = d16;
		}
	
		// receive data
		if ((d&SPI_SYNC_MASK)==SPI_SYNC_WORD)
			m_sync = true;
		else if ((d&SPI_SYNC_MASK)==SPI_SYNC_WORD_DATA)
		{
			m_rq.write(d);
			m_sync = true;
		}
		else
			m_sync = false;

		m_recvCounter++;
	}
	else
	{
		d = LPC_SSP1->DR; // grab data
		// clear interrupt
		LPC_SSP1->ICR = SSP_INTCFG_RX;  

		// fill fifo
		while(LPC_SSP1->SR&SSP_SR_TNF) 
		{
			if (m_tq.read(&d16)==0)
				d16 = 0; // stuff fifo with 0s
			LPC_SSP1->DR = d16;
		}
	
		// receive data
		if ((d&SPI_SYNC_MASK)==SPI_SYNC_WORD_DATA)
			m_rq.write(d);
	}
}

int Spi::receive(uint8_t *buf, uint32_t len)
{
	uint32_t i;
	uint16_t buf16;

	for (i=0; i<len; i++)
	{
		if (m_rq.read(&buf16)==0)
			break;
		buf[i] = buf16&0xff;
	}

	return i;
}

int Spi::receiveLen()
{	
	return m_rq.receiveLen();
}

int Spi::open()
{
	// configure SGPIO bit so we can toggle slave select (SS)
	LPC_SGPIO->OUT_MUX_CFG14 = 4;
	scu_pinmux(0x1, 3, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC5); // SSP1_MISO
	scu_pinmux(0x1, 4, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC5); // SSP1_MOSI 
	scu_pinmux(0x1, 19, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC1); // SSP1_SCK 

	// enable interrupt
	NVIC_EnableIRQ(SSP1_IRQn);

	// sync
	sync();					

	return 0;
}

int Spi::close()
{
	// turn off driver for SS
	LPC_SGPIO->GPIO_OENREG = 0;

	// disable interrupt
	NVIC_DisableIRQ(SSP1_IRQn);
	return 0;
}

int Spi::update()
{
	if (m_autoSlaveSelect)
	{
		// check to see if we've received new data (m_rq.m_produced would have increased)
		if (m_recvCounter-m_lastRecvCounter>0)
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
		m_lastRecvCounter = m_recvCounter;
	}
	return 0;
}


void Spi::setAutoSlaveSelect(bool ass)
{
	m_autoSlaveSelect = ass;
	if (m_autoSlaveSelect)
		LPC_SGPIO->GPIO_OENREG = 1<<14; // use this SGPIO Bit as slave select, so configure as output
	else
		LPC_SGPIO->GPIO_OENREG = 0; // tri-state the SGPIO bit so host can assert slave select
}
	

Spi::Spi(SerialCallback callback) : m_rq(SPI_RECEIVEBUF_SIZE), m_tq(SPI_TRANSMITBUF_SIZE, callback)
{
	uint32_t i;
	volatile uint32_t d;
	SSP_CFG_Type configStruct;

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

	NVIC_SetPriority(SSP1_IRQn, 0);	// high priority interrupt

	m_sync = false;
	m_recvCounter = 0;
	m_lastRecvCounter = 0; 
	m_syncCounter = 0;
	setAutoSlaveSelect(false);

}

void spi_init(SerialCallback callback)
{
	g_spi = new Spi(callback);
}

