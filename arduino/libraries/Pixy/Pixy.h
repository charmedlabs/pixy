/*
  Pixy.h - Library for interfacing with Pixy.
  Created by Scott Robinson, October 22, 2013.
  Released into the public domain.
*/
#ifndef PIXY_H
#define PIXY_H

#include "Arduino.h"

#define START_WORD 0xaa55
#define END_WORD 0xccaa

/*
  Credit to Gido from the Arduino forums
  for this code.
*/
template<typename Data> class vector {
   size_t d_size;		// # of stored objects
   size_t d_capacity;	// allocated capacity
   Data *d_data;		// stores data
   public:
     vector() : d_size(0), d_capacity(0), d_data(0) {};
	 
     vector(vector const &other) : d_size(other.d_size), d_capacity(other.d_capacity), d_data(0)
	 {
		d_data = (Data *)malloc(d_capacity*sizeof(Data));
		memcpy(d_data, other.d_data, d_size*sizeof(Data));
	 };
	 
     ~vector() { free(d_data); };
	 
	 // For memory management
     vector &operator=(vector const &other)
	 {
		free(d_data);
		d_size = other.d_size;
		d_capacity = other.d_capacity;
		d_data = (Data *)malloc(d_capacity*sizeof(Data));
		memcpy(d_data, other.d_data, d_size*sizeof(Data));
		return *this;
	 };
	
	 void push_back(Data const &x)
	 {
		if (d_capacity == d_size) resize();
		d_data[d_size++] = x;
	 };
	 
	 void clear()
	 {
		free(d_data);
		d_size = 0;
		d_capacity = 0;
		resize();
	 };
	
     size_t size() const
	 {
		return d_size;
	 };
	 
	 // Cons getter
     Data const &operator[](size_t idx) const
	 {
		return d_data[idx];
	 };
   
   Data const at(size_t idx) const
	 {
		return d_data[idx];
	 };
	 
	 // Changeable getter
     Data &operator[](size_t idx)
	 {
		return d_data[idx];
	 };
   private:
	 // Allocates double the old space
     void resize()
	 {
		d_capacity = d_capacity ? d_capacity*2 : 1;
		Data *newdata = (Data *)malloc(d_capacity*sizeof(Data));
		memcpy(newdata, d_data, d_size * sizeof(Data));
		free(d_data);
		d_data = newdata;
	 };
};

struct Blob {
  unsigned int model;
  unsigned int x_center;
  unsigned int y_center;
  unsigned int width;
  unsigned int height;
};

class Pixy
{
  public:
    Pixy();
    ~Pixy();
    void waitForFrame();
    vector<Blob>* getBlobs();
    vector<Blob>* getBlobs(int model);
    void setSizeLimit(unsigned int minArea, unsigned int maxArea);
  private:
    vector<Blob>* _blobs;
    unsigned int _minArea, _maxArea;
    void sync();
    unsigned int getSPIWord();
};

#endif
