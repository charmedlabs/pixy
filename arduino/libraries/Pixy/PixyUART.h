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

#ifndef _PIXYUART_H
#define _PIXYUART_H

#include "TPixy.h"
#include "Arduino.h"

class LinkUART
{
public:
  void init()
  {
    Serial1.begin(19200);
  }
  void setAddress(uint8_t addr)
  {
  }
  uint16_t getWord()
  {
    int16_t u, v;
	
	while(1)
    {
      u = Serial1.read();
      if (u>=0)
        break;
    }
    while(1)
    {
      v = Serial1.read();
      if (v>=0)
        break;
    }
    v <<= 8;
    v |= u&0xff; 
    return v;
  }
  uint8_t getByte()
  {
    int16_t u;
	
	while(1)
    {
      u = Serial1.read();
      if (u>=0)
        break;
    }
	return (uint8_t)u;
  }
};

typedef TPixy<LinkUART> PixyUART;

#endif
