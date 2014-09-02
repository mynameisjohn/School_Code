// Lab 3.5
// stepping motor simple example

// full-step table
byte step_table[] = { 0b1010, 0b1001, 0b0101, 0b0110 };

#define NSTEPS (sizeof(step_table)/sizeof(step_table[0]))
              
// pin number for motor connections (four sequential from 2..5)
byte motA = 2;

// set four outputs 2..5 for motor drivers
void setup()
{
  for( byte i=0; i<4; i++)
    pinMode( motA+i, OUTPUT);
}

int state = 0;


void loop()
{
  // run the motor for 100 steps
  for( int i=0; i<100; i++) {
    // get the motor state for the 4 bits from the table
    byte motor_bits = step_table[state];
    // set all 4 motor outputs
    for( int b=0; b<4; b++)
      digitalWrite( motA+b, (motor_bits>>b) & 1);
    state = (state + 1) % NSTEPS;
    delay( 10);
  }
  
  delay(1000);
}
