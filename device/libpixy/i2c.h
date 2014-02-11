#ifndef _I2C_H
#define _I2C_H
#include "iserial.h"
#include "lpc43xx_i2c.h"

#define I2C_DEFAULT_SLAVE_ADDR    0x54
#define I2C_TRANSMIT_BUF_SIZE     32
#define I2C_RECEIVE_BUF_SIZE      32

class I2c : public Iserial
{
public:
	I2c(LPC_I2Cn_Type *i2c, uint8_t addr, SerialCallback callback);

	// Iserial methods
	virtual int open();
	virtual int close();
	virtual int receive(uint8_t *buf, uint32_t len);
	virtual int update();

	int setSlaveAddr(uint8_t addr);
	void slaveHandler();

private:
	void startSlave();

	LPC_I2Cn_Type *m_i2c;	
	ReceiveQ<uint8_t> m_rq;
	TransmitQ<uint8_t> m_tq;
};

void i2c_init(SerialCallback callback);

extern I2c *g_i2c0;
#endif
