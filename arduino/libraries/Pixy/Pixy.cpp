/*
  Pixy.cpp - Library for interfacing with Pixy.
  Created by Scott Robinson, October 22, 2013.
  Released into the public domain.
*/
#include "Arduino.h"
#include <Pixy.h>
#include <SPI.h>

Pixy::Pixy()
{
  _blobs = new vector<Blob>();

  _minArea = 0;
  _maxArea = 0xFFFF;
  
  SPI.begin();
}

Pixy::~Pixy()
{
  delete _blobs;
  _blobs = NULL;

  SPI.end();
}

/*
  Returns all the blobs for a given frame and
  a given model.
*/
vector<Blob>* Pixy::getBlobs(int model)
{
  _blobs->clear();
  
  // Always ensure we're in sync
  unsigned int start_sync = getSPIWord();
  if (start_sync != START_WORD) {
    sync();
  }
  
  unsigned int _checksum = getSPIWord();
  unsigned int _model = getSPIWord();
  unsigned int _x_center = getSPIWord();
  unsigned int _y_center = getSPIWord();
  unsigned int _width = getSPIWord();
  unsigned int _height = getSPIWord();
  
  unsigned int end_sync = getSPIWord();
  if (end_sync != END_WORD) {
    return _blobs;
  }

  // Validate checksum
  unsigned int checksum_actual = _model + _x_center + _y_center + _width + _height;
  if (_checksum != checksum_actual) {
    return _blobs;
  }
  
  unsigned int _area = _width * _height;

  if (_area >= _minArea && _area <= _maxArea && (model < 0 || model == _model)) {
    Blob _blob = { _model, _x_center, _y_center, _width, _height };
    _blobs->push_back(_blob);
  }
  
  return _blobs;
}

vector<Blob>* Pixy::getBlobs()
{
  return this->getBlobs(-1);
}

/*
  Blocks execution until Pixy indicates
  that the current frame has ended.
*/
void Pixy::waitForFrame() {
  // Ignore everything until we find
  // the frame word
  /*unsigned int sync = getSPIWord();
  while (sync != END_WORD) {
    sync = getSPIWord();
  }*/
}

void Pixy::setSizeLimit(unsigned int minArea, unsigned int maxArea) {
  this->_minArea = minArea;
  this->_maxArea = maxArea;
}

/*
  Sync with Pixy over SPI.
*/
void Pixy::sync() {
  unsigned int start = getSPIWord();
  while (start != START_WORD) {
    start = getSPIWord();
  }
}

/*
  Retrieves a word over SPI.
*/
unsigned int Pixy::getSPIWord() {
  unsigned int xfer;
  xfer = SPI.transfer(0x00) << 8;
  xfer += SPI.transfer(0x00);
  return xfer;
}
