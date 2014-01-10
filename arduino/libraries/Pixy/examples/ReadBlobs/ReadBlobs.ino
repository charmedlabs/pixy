// Author: Scott Robinson
// charmedlabs.com
//
// Continuously prints blob data
// using the Pixy library.

#include <SPI.h>  // For some reason when SPI.h is included in a library
                  // it also needs to be included in the sketch that uses
                  // the library.
#include <Pixy.h>

Pixy* pixy;

void setup()
{
  Serial.begin(9600);
  Serial.print("Starting...\n");
  pixy = new Pixy();
}

void loop()
{ 
  vector<Blob>* blobs = pixy->getBlobs();
  
  for (int i = 0; i < blobs->size(); i++) {
    Serial.print("Color Sig: ");
    Serial.print(blobs->at(i).model);
    Serial.print("\n");
    
    Serial.print("X Center: ");
    Serial.print(blobs->at(i).x_center);
    Serial.print("\n");
    
    Serial.print("Y Center: ");
    Serial.print(blobs->at(i).y_center);
    Serial.print("\n");
    
    Serial.print("Width: ");
    Serial.print(blobs->at(i).width);
    Serial.print("\n");
    
    Serial.print("Height: ");
    Serial.print(blobs->at(i).height);
    Serial.print("\n");
    Serial.print("\n");
  }
}

