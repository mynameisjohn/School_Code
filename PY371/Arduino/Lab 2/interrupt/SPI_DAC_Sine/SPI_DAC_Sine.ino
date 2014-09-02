#include <SPI.h>
#include <TimerOne.h>

int dac_nCS = 10;            // pin number for nCS
int dac_SCK = 13;            // pin number for SCK
int dac_SDI = 11;            // pin number for SDI

volatile int dac = 0;

const float twoPi = 3.1415927 * 2.0;
static float a;
static float da = twoPi / 8;
unsigned int d;

void setup() {
  // put your setup code here, to run once:
  
  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  pinMode( dac_nCS, OUTPUT);    // define nCS as an output
  pinMode( dac_SCK, OUTPUT);
  pinMode( dac_SDI, OUTPUT);
  
  Timer1.initialize(250);
  Timer1.attachInterrupt(timerFunc);
}   

void loop() {
  setDac(0);
  setDac(4095);
}

void setDac(int s) {
  
  digitalWrite( dac_nCS, 0);    // set nCS to 0 (LOW) 
  
 int I = 0x7000 + s;

 SPI.transfer(I>>8); 
 SPI.transfer((I<<8)>>8);
 
 digitalWrite( dac_nCS, 1);    // set nCS to 1 (HIGH)
    
}

void timerFunc() {
  setDac(dac);
  dac ^= 0xfff; 
}