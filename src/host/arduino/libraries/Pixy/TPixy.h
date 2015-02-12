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
// This file is for defining the Block struct and the Pixy template class.
// (TPixy).  TPixy takes a communication link as a template parameter so that 
// all communication modes (SPI, I2C and UART) can share the same code.  
//

#ifndef _TPIXY_H
#define _TPIXY_H

#include "Arduino.h"

// Communication/misc parameters
#define PIXY_INITIAL_ARRAYSIZE      30
#define PIXY_MAXIMUM_ARRAYSIZE      130
#define PIXY_START_WORD             0xaa55
#define PIXY_START_WORD_CC          0xaa56
#define PIXY_START_WORDX            0x55aa
#define PIXY_MAX_SIGNATURE          7
#define PIXY_DEFAULT_ARGVAL         0xffff

// Pixy x-y position values
#define PIXY_MIN_X                  0L
#define PIXY_MAX_X                  319L
#define PIXY_MIN_Y                  0L
#define PIXY_MAX_Y                  199L

// RC-servo values
#define PIXY_RCS_MIN_POS            0L
#define PIXY_RCS_MAX_POS            1000L
#define PIXY_RCS_CENTER_POS         ((PIXY_RCS_MAX_POS-PIXY_RCS_MIN_POS)/2)

 
enum BlockType
{
	NORMAL_BLOCK,
	CC_BLOCK
};

struct Block 
{
  // print block structure!
  void print()
  {
    int i, j;
    char buf[128], sig[6], d;
	bool flag;	
    if (signature>PIXY_MAX_SIGNATURE) // color code! (CC)
	{
      // convert signature number to an octal string
      for (i=12, j=0, flag=false; i>=0; i-=3)
      {
        d = (signature>>i)&0x07;
        if (d>0 && !flag)
          flag = true;
        if (flag)
          sig[j++] = d + '0';
      }
      sig[j] = '\0';	
      sprintf(buf, "CC block! sig: %s (%d decimal) x: %d y: %d width: %d height: %d angle %d\n", sig, signature, x, y, width, height, angle);
    }			
	else // regular block.  Note, angle is always zero, so no need to print
      sprintf(buf, "sig: %d x: %d y: %d width: %d height: %d\n", signature, x, y, width, height);		
    Serial.print(buf); 
  }
  uint16_t signature;
  uint16_t x;
  uint16_t y;
  uint16_t width;
  uint16_t height;
  uint16_t angle;
};



template <class LinkType> class TPixy
{
public:
  TPixy(uint16_t arg=PIXY_DEFAULT_ARGVAL);
  ~TPixy();
	
  uint16_t getBlocks(uint16_t maxBlocks=1000);
  int8_t setServos(uint16_t s0, uint16_t s1);
  int8_t setBrightness(uint8_t brightness);
  int8_t setLED(uint8_t r, uint8_t g, uint8_t b);
  void init();
  
  Block *blocks;
	
private:
  boolean getStart();
  void resize();

  LinkType link;
  boolean  skipStart;
  BlockType blockType;
  uint16_t blockCount;
  uint16_t blockArraySize;
};


template <class LinkType> TPixy<LinkType>::TPixy(uint16_t arg)
{
  skipStart = false;
  blockCount = 0;
  blockArraySize = PIXY_INITIAL_ARRAYSIZE;
  blocks = (Block *)malloc(sizeof(Block)*blockArraySize);
  link.setArg(arg);
}

template <class LinkType> void TPixy<LinkType>::init()
{
  link.init();
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
	{
      blockType = NORMAL_BLOCK;
      return true;
	}
    else if (w==PIXY_START_WORD_CC && lastw==PIXY_START_WORD)
	{
      blockType = CC_BLOCK;
      return true;
	}
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
  blockArraySize += PIXY_INITIAL_ARRAYSIZE;
  blocks = (Block *)realloc(blocks, sizeof(Block)*blockArraySize);
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
	  blockType = NORMAL_BLOCK;
	  //Serial.println("skip");
      return blockCount;
    }
	else if (checksum==PIXY_START_WORD_CC)
	{
	  skipStart = true;
	  blockType = CC_BLOCK;
	  return blockCount;
	}
    else if (checksum==0)
      return blockCount;
    
	if (blockCount>blockArraySize)
		resize();
	
	block = blocks + blockCount;
	
    for (i=0, sum=0; i<sizeof(Block)/sizeof(uint16_t); i++)
    {
	  if (blockType==NORMAL_BLOCK && i>=5) // skip 
	  {
		block->angle = 0;
		break;
	  }
      w = link.getWord();
      sum += w;
      *((uint16_t *)block + i) = w;
    }

    if (checksum==sum)
      blockCount++;
    else
      Serial.println("cs error");
	
	w = link.getWord();
	if (w==PIXY_START_WORD)
	  blockType = NORMAL_BLOCK;
	else if (w==PIXY_START_WORD_CC)
	  blockType = CC_BLOCK;
	else
      return blockCount;
  }
}

template <class LinkType> int8_t TPixy<LinkType>::setServos(uint16_t s0, uint16_t s1)
{
  uint8_t outBuf[6];
   
  outBuf[0] = 0x00;
  outBuf[1] = 0xff; 
  *(uint16_t *)(outBuf + 2) = s0;
  *(uint16_t *)(outBuf + 4) = s1;
  
  return link.send(outBuf, 6);
}

template <class LinkType> int8_t TPixy<LinkType>::setBrightness(uint8_t brightness)
{
  uint8_t outBuf[3];
   
  outBuf[0] = 0x00;
  outBuf[1] = 0xfe; 
  outBuf[2] = brightness;
  
  return link.send(outBuf, 3);
}

template <class LinkType> int8_t TPixy<LinkType>::setLED(uint8_t r, uint8_t g, uint8_t b)
{
  uint8_t outBuf[5];
  
  outBuf[0] = 0x00;
  outBuf[1] = 0xfd; 
  outBuf[2] = r;
  outBuf[3] = g;
  outBuf[4] = b;
  
  return link.send(outBuf, 5);
}

#endif
