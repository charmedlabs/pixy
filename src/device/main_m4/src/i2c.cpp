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

#include <inttypes.h>
#include "lpc43xx_scu.h"
#include "i2c.h"

I2c *g_i2c0;
static uint8_t g_toCount = 0;

extern "C" void I2C0_IRQHandler(void);

void I2C0_IRQHandler(void)
{
	g_i2c0->slaveHandler();
}

void I2c::slaveHandler()
{
	uint8_t stat, c;

	g_toCount = 0; // activity, reset timeout

	stat = m_i2c->STAT&I2C_STAT_CODE_BITMASK;
	switch (stat)
	{
	// No status information 
	case I2C_I2STAT_NO_INF:
		m_i2c->CONSET = I2C_I2CONSET_AA;
		m_i2c->CONCLR = I2C_I2CONCLR_SIC;
		break;

	// Reading phase 
	// Own SLA+R has been received, ACK has been returned 
	case I2C_I2STAT_S_RX_SLAW_ACK:
	// General call address has been received, ACK has been returned 
	case I2C_I2STAT_S_RX_GENCALL_ACK:
		m_i2c->CONSET = I2C_I2CONSET_AA;
		m_i2c->CONCLR = I2C_I2CONCLR_SIC;
		break;

	// Previously addressed with own SLA
	// DATA byte has been received
	// ACK has been returned 
	case I2C_I2STAT_S_RX_PRE_SLA_DAT_ACK:
	// DATA has been received, ACK hasn been return 
	case I2C_I2STAT_S_RX_PRE_GENCALL_DAT_ACK:
		 // All data bytes that over-flow the specified receive
		 // data length, just ignore them.
		m_rq.write((uint8_t)m_i2c->DAT);
		m_i2c->CONSET = I2C_I2CONSET_AA;
		m_i2c->CONCLR = I2C_I2CONCLR_SIC;
		break;

	// Previously addressed with own SLA
	// DATA byte has been received
	// NOT ACK has been returned 
	case I2C_I2STAT_S_RX_PRE_SLA_DAT_NACK:
	// DATA has been received, NOT ACK has been returned 
	case I2C_I2STAT_S_RX_PRE_GENCALL_DAT_NACK:
	case I2C_I2STAT_S_RX_STA_STO_SLVREC_SLVTRX:
		m_i2c->CONSET = I2C_I2CONSET_AA;
		m_i2c->CONCLR = I2C_I2CONCLR_SIC;
		break;

	// Writing phase
	// Own SLA+R has been received, ACK has been returned 
	case I2C_I2STAT_S_TX_SLAR_ACK:
	// Data has been transmitted, ACK has been received 
	case I2C_I2STAT_S_TX_DAT_ACK:
		if (m_pad0 && m_16bit) // m_pad0 is there so we make sure to send 0's in pairs so we don't get out of byte-sync
		{
			m_i2c->DAT = 0;
			m_pad0 = false;
		}
		else if (m_tq.read(&c))
			m_i2c->DAT = c;
		else 
		{
			m_i2c->DAT = 0;
			m_pad0 = true;
		}
		m_i2c->CONSET = I2C_I2CONSET_AA;
		m_i2c->CONCLR = I2C_I2CONCLR_SIC;
		break;

	case I2C_I2STAT_S_TX_LAST_DAT_ACK:
	case I2C_I2STAT_S_TX_DAT_NACK:
		if (m_clearOnEnd)
		{
			m_tq.clear();
			m_rq.clear();
		}
		m_i2c->CONSET = I2C_I2CONSET_AA;
		m_i2c->CONCLR = I2C_I2CONCLR_SIC;
		break;

	// Other status must be captured
	default:
		m_i2c->CONCLR = I2C_I2CONCLR_AAC | I2C_I2CONCLR_SIC | I2C_I2CONCLR_STAC;
		break;
	}
}

int I2c::open()
{
	// make all pins on the I/O connector high impedance so we can just daisy chain the whole connector together
	scu_pinmux(0x1, 3, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 
	scu_pinmux(0x2, 1, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         
	scu_pinmux(0x2, 1, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         
	scu_pinmux(0x1, 4, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC0); 	         
	scu_pinmux(0x2, 0, (MD_PLN | MD_EZI | MD_ZI | MD_EHS), FUNC4); 	         
	// turn off driver for SS signal so we can wire-or them together
	LPC_SGPIO->GPIO_OENREG = 0;

 	m_pad0 = false;

	NVIC_EnableIRQ(I2C0_IRQn);
	startSlave(); 


	return 0;
}

int I2c::close()
{
	NVIC_DisableIRQ(I2C0_IRQn);
	return 0;
}

int I2c::receive(uint8_t *buf, uint32_t len)
{
	uint32_t i;

	for (i=0; i<len; i++)
	{
		if (m_rq.read(buf+i)==0)
			break;
	}

	return i;
}

int I2c::receiveLen()
{
	return m_rq.receiveLen();
}


int I2c::update()
{
	// try to recover from out of sync condition between slave (us) and master
	// happens when cable is unplugged/plugged
	if ((m_i2c->CONSET&I2C_I2CONSET_AA)==0)
		startSlave();
	// some events (or lack of events) can cause bus lockup -- this timeout mechanism resets if we haven't received activity 
	else if (m_i2c->STAT==I2C_I2STAT_NO_INF) // no activity
	{
		if (g_toCount>50)
		{
	    	m_i2c->CONCLR = (I2C_I2CONCLR_AAC |I2C_I2CONCLR_SIC | I2C_I2CONCLR_STAC | I2C_I2CONCLR_I2ENC);
			g_toCount = 0;
		}
		else
			g_toCount++;
	}

	return 0;
}


void I2c::startSlave()
{
	m_i2c->CONCLR = I2C_I2CONCLR_SIC | I2C_I2CONCLR_STOC | I2C_I2CONCLR_STAC;
	m_i2c->CONSET = I2C_I2CONSET_AA | I2C_I2CONSET_I2EN;
}

I2c::I2c(LPC_I2Cn_Type *i2c, uint8_t addr, SerialCallback callback) : m_rq(I2C_RECEIVE_BUF_SIZE), m_tq(I2C_TRANSMIT_BUF_SIZE, callback)
{
	m_i2c = i2c;

	m_16bit = true;
	m_clearOnEnd = false;
	 
	I2C_Init(m_i2c, 100000);
   	setSlaveAddr(addr);

	NVIC_SetPriority(SSP1_IRQn, 0);	// high priority interrupt
}

int I2c::setSlaveAddr(uint8_t addr)
{
	I2C_OWNSLAVEADDR_CFG_Type slaveAddr;
	slaveAddr.SlaveAddr_7bit = addr;
	slaveAddr.SlaveAddrMaskValue = 0;
	slaveAddr.SlaveAddrChannel = 0;
	I2C_SetOwnSlaveAddr(m_i2c, &slaveAddr);

	return 0;
}

void I2c::setFlags(bool clearOnEnd, bool sixteenBit)
{
	m_clearOnEnd = clearOnEnd;
	m_16bit = sixteenBit;
}

void i2c_init(SerialCallback callback)
{
	g_i2c0 = new I2c(LPC_I2C0, I2C_DEFAULT_SLAVE_ADDR, callback);
}
