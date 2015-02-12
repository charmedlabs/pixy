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
// This file is for defining the link class for SPI with Slave Select.  The 
// default communication for Arduino is through the ICSP connector, which uses
// SPI without a slave select.  The LinkSPI_SS allows you to use a slave select
// so you can share the SPI port with other devices, or use multiple Pixys. 
//
// Note, the PixySPI_SS class takes an optional argument, which is the pin 
// number of the slave select signal you wish to use.  The default pin is the 
// SS pin (used when no argument is used.)  So, for example, if you wished to 
// use pin 14 for slave select, declare like this:
//
// PixySPI_SS pixy(14);
//

#ifndef PIXYSPI_SS_H
#define PIXYSPI_SS_H

#include "TPixy.h"
#include "SPI.h"


#define PIXY_SYNC_BYTE              0x5a
#define PIXY_SYNC_BYTE_DATA         0x5b
#define PIXY_OUTBUF_SIZE            6

class LinkSPI_SS
{
  public:
    void init()
    {
      outLen = 0;
      SPI.begin();

      #ifdef __SAM3X8E__
      // DUE clock divider //
      SPI.setClockDivider(84);
      #else
      // Default clock divider //
      SPI.setClockDivider(SPI_CLOCK_DIV16);
      #endif
    }
    
    uint16_t getWord()
    {
      // ordering is different because Pixy is sending 16 bits through SPI 
      // instead of 2 bytes in a 16-bit word as with I2C
      uint16_t w;
      uint8_t c, cout = 0;
	  
	  // assert slave select
	  digitalWrite(ssPin, LOW);

      if (outLen)
      {
        w = SPI.transfer(PIXY_SYNC_BYTE_DATA);
        cout = outBuf[outIndex++];
        if (outIndex==outLen)
          outLen = 0; 
      }
      else
        w = SPI.transfer(0);

      w <<= 8;
      c = SPI.transfer(cout);
      w |= c;

	  // negate slave select
	  digitalWrite(ssPin, HIGH);
      return w;
    }
	
    uint8_t getByte() // this shouldn't be called normally
	// It should only be called if we get out of sync, but with slave select
	// we should stay in sync 
    {
	  uint8_t c;
 	  // assert slave select
	  digitalWrite(ssPin, LOW);
      c = SPI.transfer(0x00);
 	  // negate slave select
	  digitalWrite(ssPin, HIGH);
	  
	  return c;
   }
    
    int8_t send(uint8_t *data, uint8_t len)
    {
      if (len>PIXY_OUTBUF_SIZE || outLen!=0)
        return -1;
      memcpy(outBuf, data, len);
      outLen = len;
      outIndex = 0;
      return len;
    }

    void setArg(uint16_t arg)
    {
      if (arg==PIXY_DEFAULT_ARGVAL)
        ssPin = SS; // default slave select pin
	  else
	    ssPin = arg;
    }

  private:
    uint8_t outBuf[PIXY_OUTBUF_SIZE];
    uint8_t outLen;
    uint8_t outIndex;
	uint16_t ssPin;
};


typedef TPixy<LinkSPI_SS> PixySPI_SS;

#endif
