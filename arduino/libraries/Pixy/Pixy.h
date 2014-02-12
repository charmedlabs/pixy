/*
  Pixy.h - Library for interfacing with Pixy.
  Created by Scott Robinson, October 22, 2013.
  Released into the public domain.
*/

#ifndef PIXY_H
#define PIXY_H
#include "Arduino.h"
#include "SPI.h"
#include "Wire.h"

#define PIXY_INITIAL_ARRAYSIZE      30
#define PIXY_MAXIMUM_ARRAYSIZE      130
#define PIXY_SYNC_BYTE              0x5a
#define PIXY_START_WORD             0xaa55
#define PIXY_START_WORDX            0x55aa
#define PIXY_DEFAULT_ADDR           0x54  // I2C

struct Block 
{
  void print()
  {
    char buf[64];
  
    sprintf(buf, "sig: %d x: %d y: %d width: %d height: %d\n", signature, x, y, width, height);
    Serial.print(buf);  
  }
  uint16_t signature;
  uint16_t x;
  uint16_t y;
  uint16_t width;
  uint16_t height;
};

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

struct LinkI2C
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

template <class LinkType> class TPixy
{
public:
  TPixy(uint8_t addr=PIXY_DEFAULT_ADDR);
  ~TPixy();
	
  uint16_t getBlocks(uint16_t maxBlocks=1000);
  Block *blocks;
	
private:
  boolean getStart();
  void resize();

  LinkType link;
  boolean skipStart;
  uint16_t blockCount;
  uint16_t blockArraySize;
};

typedef TPixy<LinkSPI> Pixy;
typedef TPixy<LinkI2C> PixyI2C;

template <class LinkType> TPixy<LinkType>::TPixy(uint8_t addr)
{
  skipStart = false;
  blockCount = 0;
  blockArraySize = PIXY_INITIAL_ARRAYSIZE;
  blocks = (Block *)malloc(sizeof(Block)*blockArraySize);
  link.init(addr);
}

template <class LinkType> TPixy<LinkType>::~TPixy()
{
  free(blocks);
}

template <class LinkType> boolean TPixy<LinkType>::getStart()
{
  uint16_t w, lastw;
 
  lastw = 0xffff;
  
  while(true)
  { 
    w = link.getWord();
    if (w==0 && lastw==0)
	{
      delayMicroseconds(10);
	  return false;
	}		
    else if (w==PIXY_START_WORD && lastw==PIXY_START_WORD)
      return true;
	else if (w==PIXY_START_WORDX)
	{
	  Serial.println("reorder");
	  link.getByte(); // resync
	}
	lastw = w; 
  }
}

template <class LinkType> void TPixy<LinkType>::resize()
{
  Block *newBlocks;
  blockArraySize += PIXY_INITIAL_ARRAYSIZE;
  newBlocks = (Block *)malloc(sizeof(Block)*blockArraySize);
  memcpy(newBlocks, blocks, sizeof(Block)*blockCount);
  free(blocks);
  blocks = newBlocks;
}  
		
template <class LinkType> uint16_t TPixy<LinkType>::getBlocks(uint16_t maxBlocks)
{
  uint8_t i;
  uint16_t w, checksum, sum;
  Block *block;
  
  if (!skipStart)
  {
    if (getStart()==false)
      return 0;
  }
  else
	skipStart = false;
	
  for(blockCount=0; blockCount<maxBlocks && blockCount<PIXY_MAXIMUM_ARRAYSIZE;)
  {
    checksum = link.getWord();
    if (checksum==PIXY_START_WORD) // we've reached the beginning of the next frame
    {
      skipStart = true;
      return blockCount;
    }
    else if (checksum==0)
      return blockCount;
    
	if (blockCount>blockArraySize)
		resize();
	
	block = blocks + blockCount;
	
    for (i=0, sum=0; i<sizeof(Block)/sizeof(uint16_t); i++)
    {
      w = link.getWord();
      sum += w;
      *((uint16_t *)block + i) = w;
    }

    if (checksum==sum)
      blockCount++;
    else
      Serial.println("cs error");
	
	w = link.getWord();
    if (w!=PIXY_START_WORD)
      return blockCount;
  }
}

#endif
