#include "lpc43xx_i2c.h"
#include "i2c.h"

static uint8_t g_char = 0;

I2c *g_i2c0;
static SerialCallback g_callback;


extern "C" void I2C0_IRQHandler(void);

void I2C0_IRQHandler(void)
{
	g_i2c0->slaveHandler();
}

void I2c::slaveHandler()
{
	uint8_t stat;

	stat = m_i2c->STAT&I2C_STAT_CODE_BITMASK;
	switch (stat)
	{
	/* No status information */
	case I2C_I2STAT_NO_INF:
		m_i2c->CONSET = I2C_I2CONSET_AA;
		m_i2c->CONCLR = I2C_I2CONCLR_SIC;
		break;

	/* Writing phase -------------------------------------------------------- */
	/* Own SLA+R has been received, ACK has been returned */
	case I2C_I2STAT_S_TX_SLAR_ACK:
	/* Data has been transmitted, ACK has been received */
	case I2C_I2STAT_S_TX_DAT_ACK:
		/*
		 * All data bytes that over-flow the specified receive
		 * data length, just ignore them.
		 */
		m_i2c->DAT = g_char++;
		m_i2c->CONSET = I2C_I2CONSET_AA;
		m_i2c->CONCLR = I2C_I2CONCLR_SIC;
		break;

	/* Data has been transmitted, NACK has been received,
	 * that means there's no more data to send, exit now */
	/*
	 * Note: Don't wait for stop event since in slave transmit mode,
	 * since there no proof lets us know when a stop signal has been received
	 * on slave side.
	 */
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
}

int I2c::close()
{
}

int I2c::receive(uint8_t *buf, uint32_t len)
{
}

int I2c::update()
{
	// try to recover from out of sync condition between slave (us) and master
	// happens when cable is unplugged/plugged
	if ((m_i2c->CONSET&I2C_I2CONSET_AA)==0)
		startSlave();
}


void I2c::startSlave()
{
	m_i2c->CONCLR = I2C_I2CONCLR_SIC | I2C_I2CONCLR_STOC | I2C_I2CONCLR_STAC;
	m_i2c->CONSET = I2C_I2CONSET_AA | I2C_I2CONSET_I2EN;
}

I2c::I2c(LPC_I2Cn_Type *i2c, uint8_t addr)
{
	m_i2c = i2c;
	m_rq.m_buf = new uint8_t[I2C_RECEIVE_BUF_SIZE];
	m_rq.m_read = 0;
	m_rq.m_write = 0;
	m_rq.m_produced = 0;
	m_rq.m_consumed = 0;

	m_tq.m_buf = new uint8_t[I2C_TRANSMIT_BUF_SIZE];
	m_tq.m_read = 0;
	m_tq.m_len = 0;
	 
	I2C_Init(m_i2c, 100000);
	I2C_IntCmd(m_i2c, (Bool)true);	

   	setSlaveAddr(addr);
	
	startSlave(); 
}

int I2c::setSlaveAddr(uint8_t addr)
{
	I2C_OWNSLAVEADDR_CFG_Type slaveAddr;
	slaveAddr.SlaveAddr_7bit = addr;
	slaveAddr.SlaveAddrMaskValue = 0;
	slaveAddr.SlaveAddrChannel = 0;
	I2C_SetOwnSlaveAddr(m_i2c, &slaveAddr);
}

void i2c_setCallback(SerialCallback callback)
{
	g_callback = callback;
}

void i2c_init()
{
	g_i2c0 = new I2c(LPC_I2C0, I2C_DEFAULT_SLAVE_ADDR);
}