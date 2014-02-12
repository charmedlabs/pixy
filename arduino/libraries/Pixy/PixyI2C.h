#ifndef _PIXYI2C_H
#define _PIXYI2C_H

#include "TPixy.h"
#include "Wire.h"

class LinkI2C
{
public:
  void init(uint8_t address)
  {
    Wire.begin();
	addr = address;
  }
  uint16_t getWord()
  {
    uint16_t w;
	uint8_t c;
	Wire.requestFrom((int)addr, 2);
    c = Wire.read();
    w = Wire.read();
    w <<= 8;
    w |= c; 
    return w;
  }
  uint8_t getByte()
  {
	Wire.requestFrom((int)addr, 1);
	return Wire.read();
  }
  
private:
  uint8_t addr;
};

typedef TPixy<LinkI2C> PixyI2C;

#endif
