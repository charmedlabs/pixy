/*
  Digital Pot Control
  
  This example controls an Analog Devices AD5206 digital potentiometer.
  The AD5206 has 6 potentiometer channels. Each channel's pins are labeled
  A - connect this to voltage
  W - this is the pot's wiper, which changes when you set it
  B - connect this to ground.
 
 The AD5206 is SPI-compatible,and to command it, you send two bytes, 
 one with the channel number (0 - 5) and one with the resistance value for the
 channel (0 - 255).  
 
 The circuit:
  * All A pins  of AD5206 connected to +5V
  * All B pins of AD5206 connected to ground
  * An LED and a 220-ohm resisor in series connected from each W pin to ground
  * CS - to digital pin 10  (SS pin)
  * SDI - to digital pin 11 (MOSI pin)
  * CLK - to digital pin 13 (SCK pin)
 
 created 10 Aug 2010 
 by Tom Igoe
 
 Thanks to Heather Dewey-Hagborg for the original tutorial, 2005
 
*/


// inslude the SPI library:
#include <SPI.h>



void setup() {
   SPI.setClockDivider(SPI_CLOCK_DIV16);
   SPI.begin(); 
   Serial.begin(9600);
   Serial.print("hello\n");
}

void loop() {
  char buf[8];
  uint16_t a, b, c;
  while(1)
  {
    //delayMicroseconds(30);
    a = SPI.transfer(0xa5);
    a <<= 8;
    a += SPI.transfer(0xa5);
    b = SPI.transfer(0xa5);
    b <<= 8;
    b += SPI.transfer(0xa5);
#if 1    
    c = SPI.transfer(0xa5);
    c <<= 8;
    c += SPI.transfer(0xa5);
#endif    
    sprintf(buf, "%x\n", a);
    Serial.print(buf);
    sprintf(buf, "%x\n", b);
    Serial.print(buf);
    sprintf(buf, "%x\n", c);
    Serial.print(buf);
    
    //Serial.println(a, HEX);
    //Serial.println(b, HEX);
    //Serial.println(c, HEX);
  }
}

