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

word state = 0;
int a = 100;

void loop()
{
  run(-200);
  a+=30;
}

void run(int steps)
{
  int inc = 1;
  if (steps<0){
    steps=-steps;
    inc=-1;
}
  // run the motor for 100 steps
  for( int i=0; i<steps; i++) {
    // get the motor state for the 4 bits from the table
    byte motor_bits = step_table[state%NSTEPS];
    // set all 4 motor outputs
    for( int b=0; b<4; b++)
      digitalWrite( motA+b, (motor_bits>>b) & 1);
    state = state + inc;
    delay( 2000/(a));
  }
  if (a>400)
    a=100;
}
