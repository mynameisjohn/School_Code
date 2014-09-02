#include <TimerOne.h>

int val=0,analogPin=0;
int counter=0;

const unsigned int sample_interval_us = 100;
const unsigned int record_length = 100;

int waveform[record_length];
volatile int num_samples=0;

void setup()
{
  Serial.begin(9600);
  Timer1.initialize(sample_interval_us);
  Timer1.attachInterrupt(timerFunc);
    // set prescale to 16

 ADCSRA |= _BV( ADPS2);

 ADCSRA &= ~_BV( ADPS1);
 ADCSRA &= ~_BV( ADPS0);
}

void loop()
{
  if (Serial.read()!=-1){
    num_samples=0;
    while(num_samples < record_length){}
    for (int i=0;i<100;i++)
      Serial.println(waveform[i]);
  }
}

void timerFunc()
{
  if( num_samples < record_length){
        waveform[num_samples++] = analogRead(analogPin);
  }
}
