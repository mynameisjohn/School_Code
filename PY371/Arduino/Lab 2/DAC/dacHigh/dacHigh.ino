int dac_nCS = 10;            // pin number for nCS
int dac_SCK = 13;            // pin number for SCK
int dac_SDI = 11;            // pin number for SDI

int s = 0;

void setup() {                // setup function runs once

 pinMode( dac_nCS, OUTPUT);    // define nCS as an output
 pinMode( dac_SCK, OUTPUT);
 pinMode( dac_SDI, OUTPUT);
 
 digitalWrite( dac_nCS, 1);
 digitalWrite( dac_SCK, 0);
}

void loop() {
 setDac();
}

void setDac(){
 digitalWrite( dac_nCS, 0);    // set nCS to 0 (LOW) 
 
 if (s>4095)
  s=0; 
 
 s+=20;
     
 int I = 28762 + s;

 for (int b=15;b>=0;b--){
      digitalWrite( dac_SDI, ((I & (1<<b)) && 1));
   digitalWrite( dac_SCK, 1);
   digitalWrite( dac_SCK, 0);
 }

 digitalWrite( dac_nCS, 1);    // set nCS to 1 (HIGH)
}
