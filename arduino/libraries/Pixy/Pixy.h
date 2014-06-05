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

/*
  Pixy.h - Library for interfacing with Pixy.
  Created by Scott Robinson, October 22, 2013.
  Released into the public domain.

  06.04.2014 v0.1.3 John Leimon 
    + LinkSPI.init() should be called from the setup() 
      function instead of being called automatically from
      the TPixy<LinkSPI> constructor in global scope. This
      is a workaround for a bug (?) in the Arduino DUE in which
      calling SPI.begin() from global scope (via a constructor)
      inhibits the operation of the Serial peripheral in the
      DUE. [As of: Arduino 1.5.6-r2]
*/

#ifndef PIXY_H
#define PIXY_H

#include "TPixy.h"
#include "SPI.h"


#define PIXY_SYNC_BYTE              0x5a
#define PIXY_SYNC_BYTE_DATA         0x5b
#define PIXY_OUTBUF_SIZE            6

class LinkSPI
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

      if (outLen)
      {
        w = SPI.transfer(PIXY_SYNC_BYTE_DATA);
        cout = outBuf[outIndex++];
        if (outIndex==outLen)
          outLen = 0; 
      }
      else
        w = SPI.transfer(PIXY_SYNC_BYTE);
      w <<= 8;
      c = SPI.transfer(cout);
      w |= c;

      return w;
    }

    uint8_t getByte()
    {
      return SPI.transfer(0x00);
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

    void setAddress(uint8_t addr)
    {
      addr_ = addr;
    }

  private:
    uint8_t outBuf[PIXY_OUTBUF_SIZE];
    uint8_t outLen;
    uint8_t outIndex;
    uint8_t addr_;
};


typedef TPixy<LinkSPI> Pixy;

#endif
