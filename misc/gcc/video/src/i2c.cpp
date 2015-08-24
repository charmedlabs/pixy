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

extern "C" void I2C0_IRQHandler(void);

void I2C0_IRQHandler(void)
{
	g_i2c0->slaveHandler();
}

void I2c::slaveHandler()
{
	uint8_t stat, c;

	stat = m_i2c->STAT&I2C_STAT_CODE_BITMASK;
	switch (stat)
	{
	// No status information 
	case I2C_I2STAT_NO_INF:
		m_i2c->CONSET = I2C_I2CONSET_AA;
		m_i2c->CONCLR = I2C_I2CONCLR_SIC;
		break;

	// Writing phase
	// Own SLA+R has been received, ACK has been returned 
	case I2C_I2STAT_S_TX_SLAR_ACK:
	// Data has been transmitted, ACK has been received */
	case I2C_I2STAT_S_TX_DAT_ACK:
		if (m_tq.read(&c))
			m_i2c->DAT = c;
		else 
			m_i2c->DAT = 0;
		m_i2c->CONSET = I2C_I2CONSET_AA;
		m_i2c->CONCLR = I2C_I2CONCLR_SIC;
		break;

	case I2C_I2STAT_S_TX_LAST_DAT_ACK:
	case I2C_I2STAT_S_TX_DAT_NACK:
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
	return 0;
}

int I2c::update()
{
	// try to recover from out of sync condition between slave (us) and master
	// happens when cable is unplugged/plugged
	if ((m_i2c->CONSET&I2C_I2CONSET_AA)==0)
		startSlave();

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

void i2c_init(SerialCallback callback)
{
	g_i2c0 = new I2c(LPC_I2C0, I2C_DEFAULT_SLAVE_ADDR, callback);
}
