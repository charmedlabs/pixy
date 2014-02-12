/*
  Pixy.h - Library for interfacing with Pixy.
  Created by Scott Robinson, October 22, 2013.
  Released into the public domain.
*/

#ifndef PIXY_H
#define PIXY_H

#include "TPixy.h"
#include "SPI.h"


#define PIXY_SYNC_BYTE              0x5a

class LinkSPI
{
public:
  void init(uint8_t addr)
  {
    SPI.setClockDivider(SPI_CLOCK_DIV16);
    SPI.begin(); 
  }
  uint16_t getWord()
  {
    // ordering is different because Pixy is sending 16 bits through SPI 
	// instead of 2 bytes in a 16-bit word as with I2C
    uint16_t w;
	uint8_t c;
    w = SPI.transfer(PIXY_SYNC_BYTE);
    w <<= 8;
    c = SPI.transfer(0x00);
    w |= c;
	
    return w;
  }
  uint8_t getByte()
  {
	return SPI.transfer(0x00);
  }
};


typedef TPixy<LinkSPI> Pixy;

#endif
