int dac_nCS = 10;            // pin number for nCS

void setup() {                // setup function runs once

 pinMode( dac_nCS, OUTPUT);    // define nCS as an output

}

void loop() {

 digitalWrite( dac_nCS, 0);    // set nCS to 0 (LOW)

 digitalWrite( dac_nCS, 1);    // set nCS to 1 (HIGH)

}
