#include <SPI.h>

int dac_nCS = 10;            // pin number for nCS
int dac_SCK = 13;            // pin number for SCK
int dac_SDI = 11;            // pin number for SDI

int s = 0;

void setup() {
  // put your setup code here, to run once:
  
  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  pinMode( dac_nCS, OUTPUT);    // define nCS as an output
  pinMode( dac_SCK, OUTPUT);
  pinMode( dac_SDI, OUTPUT);
}

void loop() {
  digitalWrite( dac_nCS, 0);    // set nCS to 0 (LOW) 
  
  if (s>255)
   s=0;
  s+=16;

 SPI.transfer(112); 
 SPI.transfer(255);
 
 digitalWrite( dac_nCS, 1);    // set nCS to 1 (HIGH)
  
}
