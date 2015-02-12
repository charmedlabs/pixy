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
// This sketch is demonstrates the setServos() function.  Running this sketch
// will move the servos to their limits, back and forth, back and forth. 
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
  Serial.println("Moving pan-tilt to max positions");
  pixy.setServos(PIXY_RCS_MAX_POS, PIXY_RCS_MAX_POS);
  delay(1000);

  Serial.println("Moving pan-tilt to min positions");
  pixy.setServos(PIXY_RCS_MIN_POS, PIXY_RCS_MIN_POS);
  delay(1000);
}


