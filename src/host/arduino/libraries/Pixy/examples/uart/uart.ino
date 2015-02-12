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
// This sketch is like hello_world but uses UART communications.  If you're
// not sure what UART is, run the hello_world sketch!
//
// Note, the default baudrate for Pixy's UART communications is 19200.  Given 
// the slow datarate and Arduino's shallow serial FIFO, this sletch sometimes
// gets checksum errors, when more than 1 block is present.  This is because
// printing more than 1 object block to the serial console (as this sketch does) 
// causes the Arduino's serial FIFO to overrun, which leads to communication 
// errors.  
//

#include "PixyUART.h"


PixyUART pixy;

void setup()
{
  Serial.begin(9600); // 9600 baud for the serial *console* (not for the UART connected to Pixy)
  Serial.print("Starting...\n");
  
  pixy.init();
}

void loop()
{
  static int i = 0;
  int j;
  uint16_t blocks;
  char buf[32]; 
  
  blocks = pixy.getBlocks();
  
  if (blocks)
  {
    i++;
    
   // do this (print) every 50 frames because printing every
   // frame would bog down the Arduino
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
