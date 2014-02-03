/*
  Pixy.h - Library for interfacing with Pixy.
  Created by Scott Robinson, October 22, 2013.
  Released into the public domain.
*/
#ifndef PIXY_H
#define PIXY_H
#include "Arduino.h"

#define PIXY_INITIAL_ARRAYSIZE      30
#define PIXY_MAXIMUM_ARRAYSIZE      130
#define PIXY_SYNC_BYTE              0x5a
#define PIXY_START_WORD             0xaa55

class Block 
{
public:
  void print();
  uint16_t signature;
  uint16_t x;
  uint16_t y;
  uint16_t width;
  uint16_t height;
};

class Pixy
{
public:
  Pixy();
  ~Pixy();
	
  uint16_t getBlocks(uint16_t maxBlocks=1000);
  Block *blocks;
	
private:
  boolean getStart();
  uint16_t getWord();
  void resize();
	
  boolean skipStart;
  uint16_t blockCount;
  uint16_t blockArraySize;
};

#endif
