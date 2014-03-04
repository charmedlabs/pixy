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

#ifndef _SPI_H
#define _SPI_H
#include "lpc43xx_ssp.h"
#include "iserial.h"

#define SPI_RECEIVEBUF_SIZE   	16
#define SPI_TRANSMITBUF_SIZE  	16

#define SS_ASSERT()  			LPC_SGPIO->GPIO_OUTREG = 0;
#define SS_NEGATE() 			LPC_SGPIO->GPIO_OUTREG = 1<<14;

#define SPI_SYNC_MASK 			0xff00
#define SPI_SYNC_WORD			0x5a00
#define SPI_SYNC_WORD_DATA		0x5b00
#define SPI_MIN_SYNC_COUNT      5

class Spi : public Iserial
{
public:
	Spi(SerialCallback callback);

	// Iserial methods
	virtual int open();
	virtual int close();
	virtual int receive(uint8_t *buf, uint32_t len);
	virtual int receiveLen();
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
