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
// This sketch is demonstrates the setLED() function.  Running this sketch
// will cycle the Pixy's RGB LED through its colors.   
//

#include <SPI.h>  
#include <Pixy.h>

Pixy pixy;

void setup()
{
  Serial.begin(9600);
  Serial.print("Starting...\n");

  pixy.init();
}

void loop() 
{ 
   uint32_t i=0;
   uint8_t r, g, b;
   
   while(1)
   {
     // calculate r, g, b such that it cycles through the colors
     r = i&0xff;
     g = (i*3)&0xff;
     b = (i/3)&0xff;
     pixy.setLED(r, g, b);
     // We need to delay here because serial requests are handled 
     // every frame period (20ms). If we don't delay, we'll
     // overrun Pixy's receive queue. But that's all OK because 
     // we normally only update the LED once per frame anyway.
     delay(20);
     
     i++;
   }
}


