/*
  Pixy.cpp - Library for interfacing with Pixy.
  Created by Scott Robinson, October 22, 2013.
  Released into the public domain.
*/
#include <Pixy.h>
#include <SPI.h>


Pixy::Pixy()
{
  skipStart = false;
  blockCount = 0;
  blockArraySize = PIXY_INITIAL_ARRAYSIZE;
  blocks = (Block *)malloc(sizeof(Block)*blockArraySize);
  SPI.setClockDivider(SPI_CLOCK_DIV16);
  SPI.begin(); 
}

Pixy::~Pixy()
{
  free(blocks);
}

boolean Pixy::getStart()
{
  uint16_t w, lastw;
 
  lastw = 0xffff;
  
  while(true)
  { 
    w = getWord();
    if (w==0 && lastw==0)
	{
      delayMicroseconds(10);
	  return false;
	}		
    else if (w==PIXY_START_WORD && lastw==PIXY_START_WORD)
      return true;
      
	lastw = w; 
  }
}

uint16_t Pixy::getWord()
{
  uint16_t w;
  //char buf[16];
  w = SPI.transfer(PIXY_SYNC_BYTE);
  w <<= 8;
  w |= SPI.transfer(0x00);

  return w;
}

void Block::print()
{
  char buf[64];
  
  sprintf(buf, "sig: %d x: %d y: %d width: %d height: %d\n", signature, x, y, width, height);
  Serial.print(buf);  
}

void Pixy::resize()
{
  Block *newBlocks;
  blockArraySize += PIXY_INITIAL_ARRAYSIZE;
  newBlocks = (Block *)malloc(sizeof(Block)*blockArraySize);
  memcpy(newBlocks, blocks, sizeof(Block)*blockCount);
  free(blocks);
  blocks = newBlocks;
}  
		
uint16_t Pixy::getBlocks(uint16_t maxBlocks)
{
  uint8_t i;
  uint16_t w, checksum, sum;
  Block *block;
  
  for(blockCount=0; blockCount<maxBlocks && blockCount<PIXY_MAXIMUM_ARRAYSIZE;)
  {
    checksum = getWord();
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
      w = getWord();
      sum += w;
      *((uint16_t *)block + i) = w;
    }

    if (checksum==sum)
      blockCount++;
    else
      Serial.println("cs error");
	
	w = getWord();
    if (w!=PIXY_START_WORD)
      return blockCount;
  }
}