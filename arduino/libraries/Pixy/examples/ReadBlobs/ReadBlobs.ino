// Author: Scott Robinson
// charmedlabs.com
//
// Continuously prints blob data
// using the Pixy library.

#include <SPI.h>  // For some reason when SPI.h is included in a library
                  // it also needs to be included in the sketch that uses
                  // the library.
#include <Wire.h>
#include <Pixy.h>

Pixy pixy;

void setup()
{

  Serial.begin(9600);
  Serial.print("Starting...\n");
}

void loop()
{ 
  static int i = 0;
  int j;
  uint16_t blocks;
  char buf[16]; 
  
  blocks = pixy.getBlocks();
  
  if (blocks)
  {
    i++;
    
    if (i%50==0)
    {
      sprintf(buf, "Detected %d:\n", blocks);
      Serial.print(buf);
      for (j=0; j<blocks; j++)
      {
        sprintf(buf, "  block %d: ", j);
        Serial.print(buf); 
        pixy.blocks[j].print();
      }
    }
  }  
}

