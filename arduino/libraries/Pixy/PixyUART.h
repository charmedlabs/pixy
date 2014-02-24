#ifndef _PIXYUART_H
#define _PIXYUART_H

#include "TPixy.h"
#include "Arduino.h"

class LinkUART
{
public:
  void init(uint8_t address)
  {
    Serial1.begin(19200);
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
