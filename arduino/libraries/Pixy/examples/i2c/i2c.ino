// Author: Scott Robinson
// charmedlabs.com
//
// Continuously prints blob data
// using the Pixy library.

#include <SPI.h>  // For some reason when SPI.h is included in a library
                  // it also needs to be included in the sketch that uses
                  // the library.
#include <Pixy.h>
#include <Wire.h>


// this function is registered as an event, see setup()
void requestEvent()
{
  Wire.write("hello "); // respond with message of 6 bytes
                       // as expected by master
}

//Pixy pixy;

void setup()
{
#if 0 
  Wire.begin(0x53);
  Wire.onRequest(requestEvent);
#endif
#if 1
  Wire.begin();
#endif
  Serial.begin(9600);
  Serial.print("Starting...\n");
}

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
#if 0
  delay(100);
#endif
}

