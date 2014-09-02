// Lab 5.10
// Oscilloscope with parameter setting
// Snnnn - set sample interval to nnnn us
// Nnnnn - set record length to nnnn samples
// Q     - query settings
// T     - trigger

#include <TimerOne.h>

#define FASTADC

unsigned int sample_interval_us = 100;      // sample interval
unsigned int record_length = 100;           // number of points to save

#define MAXWF 512
int waveform[MAXWF];                                // waveform buffer
volatile int num_samples;                         // number of samples in buffer

// buffer for serial input from computer
#define NBUF 10
char ser_buff[NBUF];

void setup() {
  Serial.begin( 9600);
  Serial.setTimeout( 60000);
  Timer1.initialize( sample_interval_us);
  Timer1.attachInterrupt( timer_func);
  
#ifdef FASTADC
  // set prescale to 16
  ADCSRA |= _BV( ADPS2);
  ADCSRA &= ~_BV( ADPS1);
  ADCSRA &= ~_BV( ADPS0);
#endif
}

// timer interrupt
void timer_func() {
  if( num_samples < record_length)
    waveform[num_samples++] = analogRead( A0);
}

void loop() {  

  Serial.print('>');
  while( !Serial.available()) { }                  // wait for data from computer
  int nb = Serial.readBytesUntil( '\n', ser_buff, NBUF);    // read an entire line
  ser_buff[nb] = '\0';

  char cmd = toupper( ser_buff[0]);      // command character
  int n = atoi( &ser_buff[1]);           // integer number which may come next

  switch( cmd) {
    case 'S':                        // S means set sample rate in us
      if( n < 10 || n > 10000)
        Serial.println( F("Error1"));
      else {
        sample_interval_us = n;
        Timer1.setPeriod( n);
      }
      break;
      
    case 'N':                        // N means set record length
      if( n < 2 || n > MAXWF)
        Serial.println( F("Error2"));
      else
        record_length = n;
      break;
      
    case 'T':                        // T means trigger
      // trigger and wait for the waveform to be captured
      num_samples = 0;
      while( num_samples < record_length) { }
      // send sample count first
      Serial.println( record_length);
      for( int i=0; i<record_length; i++)
        Serial.println( waveform[i]);  
      break;

    case 'Q':                        // Q means query settings      
      Serial.print( F("Interval (us):"));
      Serial.print( sample_interval_us);
      Serial.print( F(" record len:"));
      Serial.println( record_length);
      break;
      
    default:
      Serial.println( F("Error3"));
  }
  
}

  

