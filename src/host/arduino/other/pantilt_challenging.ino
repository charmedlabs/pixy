// Author: Scott Robinson
// charmedlabs.com
//
// Continuously prints blob data
// using the Pixy library.

#include <SPI.h>  // For some reason when SPI.h is included in a library
                  // it also needs to be included in the sketch that uses
                  // the library.
#include <Pixy.h>
#include <Servo.h>

#define IMG_WIDTH 320
#define IMG_HEIGHT 200

Pixy pixy;

Servo servoPan;
Servo servoTilt;

float posX = 90;
float posY = 90;

void setup()
{
  Serial.begin(9600);
  Serial.print("Starting...\n");
  
  servoPan.attach(9);
  servoTilt.attach(10);
  
  servoPan.write(90);
  servoTilt.write(90);
}

void loop()
{ 
  static int i = 0;
  static int j = 0;
  uint16_t blocks;
  char buf[16]; 
  
  blocks = pixy.getBlocks();
  
  if (blocks)
  {
    i++;
      if (pixy.blocks[0].x > ((IMG_WIDTH/2) + 7)) {
        posX -= 1.5;
        posX = max(posX, 0);
        servoPan.write(posX);
      } else if (pixy.blocks[0].x < ((IMG_WIDTH/2) - 7)) {
        posX += 1.5;
        posX += abs(((IMG_WIDTH/2) - pixy.blocks[0].x)) / 300;
        posX = min(posX, 180);
        servoPan.write(posX);
      }
      
      if (pixy.blocks[0].y > ((IMG_HEIGHT/2) + 5)) {
        posY += 1.5;
        posY = min(posY, 180);
        servoTilt.write(posY);
      } else if (pixy.blocks[0].y < ((IMG_HEIGHT/2) - 5)) {
        posY -= 1.5;
        posY = max(posY, 0);
        servoTilt.write(posY);
      }
      
    if (i%50==0) {
      j++;
      Serial.print(j);
      Serial.print("\n");
    }
  }
}
