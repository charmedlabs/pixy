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


PixyI2C pixy;


void setup()
{
  //Wire.begin();
  Serial.begin(9600);
  Serial.print("Starting...\n");
}

#if 0
void loop()
{ 
  static unsigned int i = 0;
  int j;
  char buf[32];
  
#if 0  
  for (j=0; j<256; j++)
  {
    Wire.beginTransmission(0x01);
    Wire.write(j); // 
    Wire.endTransmission();
    delay(10);
  }
#endif
#if 1
  Wire.requestFrom(0x54, 2);
  while(Wire.available())
  {
    uint8_t c = Wire.read();
    uint16_t d = Wire.read();
    d <<= 8;
    d |= c; 
    sprintf(buf, "%x\n", d);
    //sprintf(buf, "%x %x %x\n", c);
    Serial.print(buf);
  }
#endif
}
#else
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
#endif

