#ifndef _SPI_H
#define _SPI_H
#include "lpc43xx_ssp.h"
#include "iserial.h"

#define SPI_RECEIVEBUF_SIZE   	1
#define SPI_TRANSMITBUF_SIZE  	16

#define SS_ASSERT()  			LPC_SGPIO->GPIO_OUTREG = 0;
#define SS_NEGATE() 			LPC_SGPIO->GPIO_OUTREG = 1<<14;

#define SPI_SYNC_MASK 			0xff00
#define SPI_SYNC_WORD			0x5a00
#define SPI_MIN_SYNC_COUNT      5

class Spi : public Iserial
{
public:
	Spi(SerialCallback callback);

	// Iserial methods
	virtual int open();
	virtual int close();
	virtual int receive(uint8_t *buf, uint32_t len);
	virtual int update();

	void slaveHandler();

private:
	int checkIdle();
	int sync();
	ReceiveQ<uint16_t> m_rq;
	TransmitQ<uint16_t> m_tq;

	bool m_sync;
	uint32_t m_recvCounter;
	uint32_t m_lastRecvCounter; 
	uint8_t m_syncCounter;
};

void spi_init(SerialCallback callback);

extern Spi *g_spi;

#endif
